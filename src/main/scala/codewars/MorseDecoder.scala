package codewars


object MorseDecoder {

  object MorseCodes {
    val morseCodes: Map[String, String] = Map.empty
  }
  import MorseCodes.morseCodes

  def decode(msg: String): String = {
    msg.trim.split("   ").map(_.split(" ").map(morseCodes).mkString("")).mkString(" ")
  }
}
