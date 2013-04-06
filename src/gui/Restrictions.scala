package gui

object Restrictions {

  sealed trait SlotRestrictions { def str: String }
  case object SlotI extends SlotRestrictions{ val str = "I" }
  case object SlotO extends SlotRestrictions{ val str = "O" }
  case object SlotE extends SlotRestrictions{ val str = "E" }
  case object SlotIO extends SlotRestrictions{ val str = "IO" }
  
  private val SLOT_MAP = Map[String, SlotRestrictions](
      SlotI.str -> SlotI,
      SlotO.str -> SlotO,
      SlotE.str -> SlotE,
      SlotIO.str -> SlotIO
    )
    
  def getSlotFromString( str: String ) = SLOT_MAP(str)
  
  sealed trait ModuleRestrictions { 
    def str: String
    def description: String
    def matches : Set[SlotRestrictions]
  }
  
  case object ModI extends ModuleRestrictions { 
    val str = "I"
    val description = "I or IO Only"
    val matches = Set[SlotRestrictions](SlotI, SlotIO) 
  }
  
  case object ModO extends ModuleRestrictions {
    val str = "O"
    val description = "O or IO Only"
    val matches = Set[SlotRestrictions](SlotO, SlotIO)
  }
  
  case object ModIO extends ModuleRestrictions {
    val str = "IO"
    val description = "I or O or IO"
    val matches = Set[SlotRestrictions](SlotI, SlotO, SlotIO) 
  }
  
  case object ModE extends ModuleRestrictions {
    val str = "E"
    val description = "E Only"
    val matches = Set[SlotRestrictions](SlotE) 
  }
  
  case object ModIOE extends ModuleRestrictions {
    val str = "IOE"
    val description = "I, O, IO, or E"
    val matches = Set[SlotRestrictions](SlotI, SlotO, SlotIO, SlotE)
  }
  
  private val MOD_MAP = Map[String, ModuleRestrictions](
      ModI.str -> ModI,
      ModO.str -> ModO,
      ModIO.str -> ModIO,
      ModE.str -> ModE,
      ModIOE.str -> ModIOE
    )
    
  def getModuleFromString( str: String ) = MOD_MAP(str)
}