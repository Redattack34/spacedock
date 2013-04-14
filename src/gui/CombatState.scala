package gui

sealed trait CombatState {
  def str: String
  def desc: String
  def token: Int
}

case object AttackRuns extends CombatState{ 
  val str = "AttackRuns"
  val desc = "Attack Runs" 
  val token = 200
}

case object Artillery extends CombatState{ 
  val str = "Artillery"
  val desc = str 
  val token = 201
}

case object HoldPosition extends CombatState{ 
  val str = "HoldPosition"
  val desc = "Hold Position"
  val token = 263
}

case object OrbitPort extends CombatState{ 
  val str = "OrbitLeft"
  val desc = "Orbit Port"
  val token = 202
}

case object OrbitStarboard extends CombatState{
  val str = "OrbitRight"
  val desc = "Orbit Starboard"
  val token = 203
}

case object Evade extends CombatState{
  val str = "Evade"
  val desc = str
  val token = 205
}


object CombatState {
  private val MAP : Map[String, CombatState] = Map(
        AttackRuns.str -> AttackRuns,
        Artillery.str -> Artillery,
        HoldPosition.str -> HoldPosition,
        OrbitPort.str -> OrbitPort,
        OrbitStarboard.str -> OrbitStarboard,
        Evade.str -> Evade
      )
      
  def getFromString( str : String ) : Option[CombatState] = MAP.get(str)
}