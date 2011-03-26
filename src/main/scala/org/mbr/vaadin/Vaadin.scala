package org.mbr.vaadin

import com.vaadin.ui.Window.CloseListener
import com.vaadin.ui.Button.ClickListener
import com.vaadin.event.FieldEvents.{BlurListener, BlurEvent, BlurNotifier}
import com.vaadin.ui.Component.{Event, Listener}
import com.vaadin.event.ItemClickEvent.{ItemClickListener, ItemClickNotifier}
import com.vaadin.ui._
import com.vaadin.data.Property._
import com.vaadin.event.{MouseEvents, ItemClickEvent, FieldEvents}
import com.vaadin.event.MouseEvents.{DoubleClickEvent, DoubleClickListener, ClickEvent}
import com.vaadin.ui.UriFragmentUtility.FragmentChangedListener

/**
 * @author mathias.broekelmann
 * @since 25.03.11 17:16
 */
object Vaadin {

  implicit def richWindow(window: Window) = new RichWindow(window)

  implicit def richButton(button: Button) = new RichButton(button)

  implicit def richBlurNotifier(blurNotifier: BlurNotifier) = new RichBlurNotifier(blurNotifier)

  implicit def richComponent(component: Component) = new RichComponent(component)

  implicit def richTree(tree: Tree) = new RichTree(tree)

  implicit def richItemClickNotifier(notifier: ItemClickNotifier) = new RichItemClickNotifier(notifier)

  implicit def richValueChangeNotifier(notifier: ValueChangeNotifier) = new RichValueChangeNotifier(notifier)

  implicit def richEmbedded(embedded: Embedded) = new RichEmbedded(embedded)

  implicit def richUriFragment(uriFragment: UriFragmentUtility) = new RichUriFragment(uriFragment)
}

class RichUriFragment(uriFragment: UriFragmentUtility) {

  def fragment(pf: PartialFunction[String, Unit]): FragmentChangedListener = {
    val listener = new FragmentChangedListener {
      def fragmentChanged(source: UriFragmentUtility#FragmentChangedEvent) = {
        val fragment = source.getUriFragmentUtility.getFragment
        if(pf.isDefinedAt(fragment))
          pf(fragment)
      }
    }
    uriFragment.addListener(listener)
    listener
  }
}

class MouseButton(button: Int) {
  def unapply(event: MouseEvents.ClickEvent): Option[MouseEvents.ClickEvent] = {
    if(event.getButton == button)
      Some(event)
    else
      None
  }
}

object Right extends MouseButton(MouseEvents.ClickEvent.BUTTON_RIGHT)
object Left extends MouseButton(MouseEvents.ClickEvent.BUTTON_LEFT)
object Middle extends MouseButton(MouseEvents.ClickEvent.BUTTON_MIDDLE)

class RichEmbedded(embedded: Embedded) {

  private def click(f: => Unit): MouseEvents.ClickListener = click(_ => f)

  def click(pf: PartialFunction[ClickEvent, Unit]): MouseEvents.ClickListener =
    click {
      event => if (pf.isDefinedAt(event)) pf(event)
    }

  private def click(f: MouseEvents.ClickEvent => Unit) = {
    val listener = new MouseEvents.ClickListener {
      def click(event: ClickEvent) = f(event)
    }
    embedded.addListener(listener)
    listener
  }
}

class RichValueChangeNotifier(notifier: ValueChangeNotifier) {

  def valuechange(f: PartialFunction[AnyRef, Unit]): ValueChangeListener = {
    change(event => {
      val value = event.getProperty.getValue
      if (f.isDefinedAt(value)) {
        f(value)
      }
    })
  }

  def change(f: ValueChangeEvent => Unit): ValueChangeListener = {
    val listener = new ValueChangeListener {
      def valueChange(event: ValueChangeEvent) = f(event)
    }
    notifier.addListener(listener)
    listener
  }
}

class RichItemClickNotifier(notifier: ItemClickNotifier) {

  def itemClick(f: ItemClickEvent => Unit) = {
    val listener = new ItemClickListener {
      def itemClick(event: ItemClickEvent) = f(event)
    }
    notifier.addListener(listener)
    listener
  }

}

class RichTree(tree: Tree) {

  def expand(f: (Tree, AnyRef) => Unit): Tree.ExpandListener =
    expand(event => f(event.getComponent.asInstanceOf[Tree], event.getItemId))

  def expand(f: Tree#ExpandEvent => Unit) = {
    val listener = new Tree.ExpandListener {
      def nodeExpand(event: Tree#ExpandEvent) = f(event)
    }
    tree.addListener(listener)
    listener
  }
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