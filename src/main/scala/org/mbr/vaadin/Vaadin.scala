package org.mbr.vaadin

import com.vaadin.ui.{Component, Button, Window}
import com.vaadin.ui.Window.CloseListener
import com.vaadin.ui.Button.ClickListener
import com.vaadin.event.FieldEvents
import com.vaadin.event.FieldEvents.{BlurListener, BlurEvent, BlurNotifier}
import com.vaadin.ui.Component.{Event, Listener}

/**
 * @author mathias.broekelmann
 * @since 25.03.11 17:16
 */
object Vaadin {

  implicit def richWindow(window: Window) = new RichWindow(window)

  implicit def richButton(button: Button) = new RichButton(button)

  implicit def richBlurNotifier(blurNotifier: BlurNotifier) = new RichBlurNotifier(blurNotifier)

  implicit def richComponent(component: Component) = new RichComponent(component)
}


class RichWindow(window: Window) {

  def close(f: => Unit): CloseListener = close(_ => f)

  def close(f: Window#CloseEvent => Unit) = {
    val listener = new CloseListener {
      def windowClose(e: Window#CloseEvent) = f(e)
    }
    window.addListener(listener)
    listener
  }
}

class RichButton(button: Button) {

  def click(f: => Unit): Button.ClickListener = click(_ => f)

  def click(f: Button#ClickEvent => Unit) = {
    val listener = new ClickListener {
      def buttonClick(event: Button#ClickEvent) = f(event)
    }
    button.addListener(listener)
    listener
  }
}

class RichBlurNotifier(blurNotifier: BlurNotifier) {

  def blur(f: => Unit): FieldEvents.BlurListener = blur(_ => f)

  def blur(f: BlurEvent => Unit) = {
    val listener = new BlurListener {
      def blur(event: BlurEvent) = f(event)
    }
    blurNotifier.addListener(listener)
    listener
  }
}

class RichComponent(component: Component) {

  def bind(f: => Unit): Listener = bind(_ => f)

  def bind(f: Component.Event => Unit) = {
    val listener = new Listener {
      def componentEvent(event: Event) = f(event)
    }
    component.addListener(listener)
    listener
  }
}