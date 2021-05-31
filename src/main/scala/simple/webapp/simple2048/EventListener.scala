package simple.webapp.simple2048

import org.scalajs.dom
import org.scalajs.dom.{Element, document}

object EventListener {

  def addKeyboardEvents(forward: Command => Unit): Unit = {
    document.addEventListener(
      "keydown", { (e: dom.KeyboardEvent) =>
        println(s"key: ${e.key}")
        val command = e.key match {
          case "ArrowDown" | "j"  => Command.Down
          case "ArrowUp" | "k"    => Command.Up
          case "ArrowLeft" | "h"  => Command.Left
          case "ArrowRight" | "l" => Command.Right
          case _                  => Command.Nop
        }
        if (command != Command.Nop) {
          forward(command)
        }
      }
    )
  }

  def addTouchEvents(svg: Element)(forward: Command => Unit): Unit = {
    val msZ = (0.0, 0.0)
    var ms  = msZ
    var mv  = false
    svg.addEventListener("touchstart", { (e: dom.TouchEvent) =>
      e.preventDefault()
      ms = (e.touches(0).clientX, e.touches(0).clientY)
    })
    svg.addEventListener("touchmove", { (e: dom.TouchEvent) =>
      mv = true
      e.preventDefault()
    })
    svg.addEventListener(
      "touchend", { (e: dom.TouchEvent) =>
        e.preventDefault()
        println(e.changedTouches.length)
        val (x0, y0) = ms
        val (x1, y1) = (e.changedTouches(0).clientX, e.changedTouches(0).clientY)
        ms = msZ
        mv = false
        if (math.abs(x1 - x0) > math.abs(y1 - y0)) {
          if (x1 > x0) {
            forward(Command.Right)
          } else {
            forward(Command.Left)
          }
        } else {
          if (y1 > y0) {
            forward(Command.Down)
          } else {
            forward(Command.Up)
          }
        }
      }
    )
  }

}
