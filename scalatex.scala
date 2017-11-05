import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes

import scala.collection.JavaConverters._
import scala.util.{Success, Try}
object scalatex extends App {

  val task: String = args(0)
  val subtask: String = args(1)
  val num: Option[Int] = Try(args(2).toInt).toOption
  val subnum: Option[Int] = Try(args(3).toInt).toOption

  task match {
    case Tasks.TASK => {
      subtask match {
        case "new" => {
          Files.createDirectories(PathRef.tasks)
          if (!Files.exists(PathRef.tasksFile)) {
            val c = List("%File containing references to tasks")
            Files.write(PathRef.tasksFile, c.asJava, StandardCharsets.UTF_8, StandardOpenOption.CREATE_NEW)
          }
          num match {
            case Some(n) => {
              1 to n foreach {_ => TaskActions.createTask}
            }
            case None => TaskActions.createTask
          }
        }
        case "rm" => {
          num match {
            case Some(n) => {
              subnum match {
                case Some(m) => {
                  Math.min(n, m) to Math.max(n, m) foreach {TaskActions.removeTask(_)}
                }
                case None => TaskActions.removeTask(n)
              }
            }
            case None => //
          }
        }
      }
    }
  }

  object TaskActions {

    private def appendTask(implicit task: Int): Boolean = {
      val lines: List[String] = Util.fileLines(PathRef.tasksFile)
      val nums: List[Int] = (task :: mapInputToTask(lines)).sorted
      val newLines: List[String] = mapTaskToInput(nums)
      val tryFile = Util.writeFile(PathRef.tasksFile, newLines)

      return tryFile.isSuccess
    }

    private def mapTaskToInput(tasks: List[Int]): List[String] = {
      tasks.map(task => "\\input{content/tasks/task%d/head.tex}".format(task))
    }

    private def mapInputToTask(l: List[String]): List[Int] = {
      l
      .filterNot(_.startsWith("%"))
      .map(_.replace("\\input{content/tasks/task", "").replace("/head.tex}", ""))
      .map(_.toInt)
    }

    private def openTask: Int = {
      val taskLines = Util.fileLines(PathRef.tasksFile)
      val tasks: List[Int] = mapInputToTask(taskLines)

      return Util.findFirstMissingNumber(tasks)
    }

    def createTask: Boolean = {
      implicit val num: Int = openTask
      val lines: List[String] = List(
        "\\section{Task %d}".format(num),
        "something something"
      )

      val tryDir = Try(Files.createDirectories(PathRef.taskFile))
      val tryFile = Util.createFile(PathRef.taskHead, Some(lines))

      (tryDir, tryFile) match {
        case (_, Success(_)) => appendTask; return true
        case (_, _) => return false
      }

    }

    def removeTask(implicit num: Int): Boolean = {
      val tryDelete = Util.deleteFolder(PathRef.taskFile)

      val lines: List[String] = Util.fileLines(PathRef.tasksFile)
      val newTasks: List[String] = mapTaskToInput(mapInputToTask(lines).filter(n => n != num))
      val tryUpdateFile: Try[Path] = Util.writeFile(PathRef.tasksFile, newTasks)

      return tryDelete.isSuccess && tryUpdateFile.isSuccess
    }

  }

  object Util {

    def deleteFolder(path: java.nio.file.Path): Try[java.nio.file.Path] = {
      Try(Files.walkFileTree(path, new FileVisitor[file.Path] {
        override def postVisitDirectory(dir: file.Path, exc: IOException): FileVisitResult = {
          if (exc == null) {
            Files.delete(dir)
            return FileVisitResult.CONTINUE
          }
          throw exc
        }

        override def visitFile(file1: file.Path, attrs: BasicFileAttributes): FileVisitResult = {
          Files.delete(file1)
          FileVisitResult.CONTINUE
        }

        override def visitFileFailed(file1: file.Path, exc: IOException) = FileVisitResult.CONTINUE

        override def preVisitDirectory(dir: file.Path, attrs: BasicFileAttributes) = FileVisitResult.CONTINUE
      }))
    }

    def writeFile(path: java.nio.file.Path, lines: List[String]): Try[java.nio.file.Path] = {
      Try(Files.write(
        path,
        lines.asJava,
        StandardCharsets.UTF_8,
        StandardOpenOption.CREATE,
        StandardOpenOption.TRUNCATE_EXISTING
      ))
    }

    def createFile(path: java.nio.file.Path, lines: Option[List[String]]): Try[java.nio.file.Path] = {
      if (!Files.exists(path)) {
        val toWrite = lines.getOrElse(List()).asJava
        return Try(Files.write(
          path,
          toWrite, StandardCharsets.UTF_8,
          StandardOpenOption.CREATE_NEW
        ))
      }
      return Success(path)
    }

    def fileLines(path: java.nio.file.Path): List[String] = {
      Files.lines(path).iterator().asScala.toList
    }

    def findFirstMissingNumber(list: List[Int]): Int = {
      def _search(list: List[(Int, Int)]): Option[Int] = {
        list match {
          case h :: t => {
            h match {
              case (n, i) => {
                if (n > (i+1)) return Some(i+1)
                return _search(t)
              }
            }
          }
          case Nil => return None
        }
      }

      _search(list.sorted.zipWithIndex) match {
        case Some(n) => return n
        case None => return list.length + 1
      }
    }

  }

  object Tasks {
    val TASK: String = "task"
  }

  object PathRef {
    val content: java.nio.file.Path = Paths.get("content")
    val tasks: Path = Paths.get("content/tasks")
    def taskFile(implicit n: Int): Path = Paths.get("content/tasks/task%d".format(n))
    def taskHead(implicit n: Int): Path = Paths.get("content/tasks/task%d/head.tex".format(n))
    val tasksFile: Path = Paths.get("content/tasks/tasks.tex")
  }

}
