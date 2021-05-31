package simple.webapp.simple2048

sealed trait Command

object Command {
  case object Up    extends Command
  case object Down  extends Command
  case object Left  extends Command
  case object Right extends Command
  case object Nop   extends Command
}
