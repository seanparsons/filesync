package com.futurenotfound.filesync

import akka.actor.Actor._
import akka.actor._
import scalaz._
import scalaz.Scalaz._
import akka.scalaz.futures._
import java.io.{FileInputStream, File}
import com.futurenotfound.filesync.IOUtil._
import java.security.{DigestInputStream, MessageDigest}

case class FileNode(filename: String, directory: Boolean, hash: Array[Byte])
sealed abstract class FileDifference
case class Added(fileNode: FileNode) extends FileDifference
case class Removed(fileNode: FileNode) extends FileDifference
case class Changed(fileNode: FileNode) extends FileDifference
case class ToDirectory(fileNode: FileNode) extends FileDifference
case class ToFile(fileNode: FileNode) extends FileDifference

object FileTrees {
  def compare(left: Set[FileNode], right: Set[FileNode]): Set[FileDifference] = {
    val leftMap = left.map(node => (node.filename, node)).toMap
    val rightMap = right.map(node => (node.filename, node)).toMap
    ((left -- right).map(fileNode => Added(fileNode)) ++
    (right -- left).map(fileNode => Removed(fileNode)) ++
    rightMap.keySet.intersect(leftMap.keySet).collect{
      case filename if (leftMap(filename).directory && !rightMap(filename).directory) => ToFile(rightMap(filename))
      case filename if (!leftMap(filename).directory && rightMap(filename).directory) => ToDirectory(rightMap(filename))
      case filename if (rightMap(filename).hash != leftMap(filename).hash) => Changed(rightMap(filename))
    })
  }

  def fromFile(file: File): Option[Tree[File]] = if (file.exists()) Some(if (file.isFile) leaf(file) else node(file, file.listFiles().flatMap(child => fromFile(child)).toStream)) else None

  def buildHash(file: File, childFiles: Stream[Tree[FileNode]]): FileNode = {
    val messageDigest  = MessageDigest.getInstance("MD5")
    if (file.isFile) {
      val inputStream = new FileInputStream(file)
      val hash = using(new DigestInputStream(inputStream, messageDigest))(digestInputStream => messageDigest.digest())
      new FileNode(file.getName, file.isDirectory, hash)
    } else {
      childFiles.map(hashFileTree => hashFileTree.rootLabel.hash).foreach(messageDigest.update)
      new FileNode(file.getName, file.isDirectory, messageDigest.digest())
    }
  }

  def getHashes(file: File): Option[Tree[FileNode]] = fromFile(file).map(_.scanr(buildHash))
}

case class StartAnalysing(path: String)
case class FinishedAnalysing(fileNodes: Option[Tree[FileNode]])
case object IsFinishedAnalysing
case class StartComparing(otherActor: ActorRef)
case class GetSubNodes(paths: List[String])
case class NodeLookupResult(nodeOption: Option[FileNode])

case class ComparisonActor(fileActor: ActorRef, remote: Boolean) extends Actor {
  def receive = {
    case startComparing: StartAnalysing => {
      fileActor ! startComparing
      become {
        case IsFinishedAnalysing => self.reply_?(false)
        case FinishedAnalysing(fileNodesOption) => {
          become {
            case IsFinishedAnalysing => self.reply_?(true)
            case GetSubNodes(path) => self.reply_?(getSubNodes(fileNodesOption, path))
            case StartComparing(otherActor) => {
              // Left: Option[Tree[FileNode]]
              // Right: Option[Tree[FileNode]] (remote)
              self.reply_?(compareNodes(otherActor, fileNodesOption, List()))
              unbecome()
            }
          }
          unbecome()
        }
      }
    }
  }

  def getRemoteNodes(otherActor: ActorRef, path: List[String]): Option[Set[FileNode]] = (otherActor !! GetSubNodes(path)).asInstanceOf[Option[Set[FileNode]]]

  def compareNodes(otherActor: ActorRef,
                   localFileNodesOption: Option[Tree[FileNode]],
                   path: List[String]): Option[Set[FileDifference]] = {
    getRemoteNodes(otherActor, path).flatMap{remoteNodes =>
      FileTrees.compare(getSubNodes(localFileNodesOption, path), remoteNodes).map{
        case changed@Changed(fileNode) => compareNodes(otherActor, localFileNodesOption, path :+ fileNode.filename)
        case fileDifference: FileDifference => Some(Set(fileDifference))
      }.foldRight(Option(Set[FileDifference]())){(left, right) =>
        left.flatMap(fileDifferences => right.map(otherFileDifferences => otherFileDifferences ++ fileDifferences))
      }
    }
  }

  def getSubNodes(localTreeOption: Option[Tree[FileNode]], paths: List[String]): Set[FileNode] = {
    if (paths.empty) localTreeOption.map(_.rootLabel).toSet
    else {
      getNode(localTreeOption, paths).toSet.flatMap[FileNode, Set[FileNode]](treeFileNode => treeFileNode.subForest.map(subNode => subNode.rootLabel).toSet)
    }
  }

  def getNode(localTreeOption: Option[Tree[FileNode]], paths: List[String]): Option[Tree[FileNode]] = {
    // A -> (B, C, D)
    // paths: List(B)
    // A -> ()
    // paths: List(B)

    // A -> (B, C, D)
    // paths: List()
    // A -> ()
    // paths: List()
    def findSubTree(tree: Tree[FileNode], paths: List[String]): Option[Tree[FileNode]] = {
      paths.headOption.map(pathHead => tree.subForest.find(subTree => subTree.rootLabel.filename == pathHead))
        // None => No paths left, return Some(tree).
        // Some(None) => Couldn't find sub tree, return None.
        // Some(Some(subTree)) => Found subtree, call recursively against subTree and paths.tail.
        .map(subTreeOption => subTreeOption.map(subTree => findSubTree(subTree, paths.tail)).getOrElse(None)).getOrElse(Some(tree))
    }
    localTreeOption.flatMap(localTree => findSubTree(localTree, paths))
  }
}

case class FileActor() extends Actor {
  def receive = {
    case StartAnalysing(path) => {
      self.sender.foreach(sender => sender ! FinishedAnalysing(FileTrees.getHashes(new File(path))))
    }
  }
}