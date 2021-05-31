package simple.webapp.simple2048

final case class Settings(colors: Map[Int, String])

object Settings {
  val settings: Settings = Settings(
    colors = Map(
      0     -> "#ecf0f1",
      2     -> "#049372",
      4     -> "#2574a9",
      8     -> "#8e44ad",
      16    -> "#e67e22",
      32    -> "#e74c3c",
      64    -> "#27ae60",
      128   -> "#f7ca18",
      256   -> "#2c3e50",
      512   -> "#d35400",
      1024  -> "#f1c40f",
      2048  -> "#8e44ad",
      4096  -> "#cf000f",
      8192  -> "#f9690e",
      16384 -> "#1e824c",
      32768 -> "#9a12b3",
      65536 -> "#9a12b3"
    )
  )
}
