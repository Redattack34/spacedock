package gui

import scala.swing.event.MouseClicked
import javax.swing.SwingUtilities
import scala.swing.event.MouseButtonEvent

class MouseClickWrapper(click: MouseButtonEvent) {
    def isRight = SwingUtilities.isRightMouseButton(click.peer)
    def isLeft = SwingUtilities.isLeftMouseButton(click.peer)
    def isMiddle = SwingUtilities.isMiddleMouseButton(click.peer)
}

object MouseClickWrapper {
  implicit def click2wrapper( clicked: MouseButtonEvent ) = new MouseClickWrapper(clicked)
}