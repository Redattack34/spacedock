package gui

import javax.swing.SwingUtilities

import scala.swing.event.MouseButtonEvent
import scala.swing.event.MouseEvent

class MouseClickWrapper(click: MouseButtonEvent) {
    def isRight = SwingUtilities.isRightMouseButton(click.peer)
    def isLeft = SwingUtilities.isLeftMouseButton(click.peer)
    def isMiddle = SwingUtilities.isMiddleMouseButton(click.peer)
}

class MouseEventWrapper(ev: MouseEvent) {
  def isShiftDown = ev.peer.isShiftDown
  def isCtrlDown = ev.peer.isControlDown
  def isAltDown = ev.peer.isAltDown
}

object MouseEventWrappers {
  implicit def click2wrapper( clicked: MouseButtonEvent ) = new MouseClickWrapper(clicked)
  implicit def event2wrapper( ev: MouseEvent ) = new MouseEventWrapper( ev )
}