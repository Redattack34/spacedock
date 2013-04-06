package gui

sealed trait CombatState {
  def str: String
}

case object AttackRuns extends CombatState{ val str = "AttackRuns" }
case object Artillery extends CombatState{ val str = "Artillery" }
case object HoldPosition extends CombatState{ val str = "HoldPosition" }
case object OrbitPort extends CombatState{ val str = "OrbitLeft" }
case object OrbitStarboard extends CombatState{ val str = "OrbitRight" }
case object Evade extends CombatState{ val str = "Evade" }

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