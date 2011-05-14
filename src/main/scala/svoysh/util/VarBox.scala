package svoysh.util

import net.liftweb.common._

/**
 * VarBox factory with case matching functionality.
 */
object VarBox {

	def apply[V](): EmptyVarBox[V] = new EmptyVarBox[V]()

	def apply[V](value: V): FullVarBox[V] = new FullVarBox[V](value)

	def apply[V](lazyValue: () => V): LazyVarBox[V] = new LazyVarBox[V](lazyValue)

	def apply[V](value: V, defaultValue: V): AnywayVarBox[V] =
		apply(value, () => defaultValue)

	def apply[V](value: V, defaultValue: () => V): AnywayVarBox[V] = {
		val varBox = new AnywayVarBox[V](defaultValue)
		varBox.set(value)
		varBox
	}

	def unapplySeq[V](varBox: VarBox[V]): Option[List[V]] = {
		varBox match {
			case null =>
			case v => return {
				if (v.isSet) Some(List(v.is))
				else Some(Nil)
			}
		}
		None
	}
}

/**
 * Kinda rewritable Box with default value.
 * If method [is] called and there is no value in box then exception
 * will be thrown: EmptyVarBoxException.
 */
sealed trait VarBox[V] {

	protected var valueBox: Box[V] = Empty

	def apply(value: V) {
		set(value)
	}

	/** For internal usage only! */
	final protected def _is : V = valueBox.open_!

	def is: V = {
		if (!_isEmpty) _is
		else throw new EmptyVarBoxException("No value in the box.")
	}

	/** Alias for method is. */
	def get: V = is

	def set(value: V) {
		valueBox = Full(value)
	}

	def clear {
		valueBox = Empty
	}

	def canBeEmpty: Boolean = true

	/** For internal usage only! */
	final protected def _isEmpty : Boolean = {
		valueBox.isEmpty
	}

	def isEmpty: Boolean = _isEmpty

	def isSet: Boolean = !isEmpty

	override def equals(other: Any): Boolean = other match {
		case null => false
		case o if !o.isInstanceOf[VarBox[V]] => false
		case o: VarBox[V] => (this eq o) || (isEmpty && o.isEmpty) ||
			(isSet && o.isSet && is == o.is)
		case _ => false
	}

	override def toString: String = {
		val valStr = if (isEmpty) "<Empty>" else "value = " + is
		"VarBox(" + valStr + ")"
	}
}

class FullVarBox[V](value: V) extends VarBox[V] {
	set(value)
}

class EmptyVarBox[V]() extends VarBox[V]

/**
 * The box that cannot be empty.
 */
sealed abstract class CannotBeEmptyVarBox[V] extends EmptyVarBox[V]() {
	override def canBeEmpty = false
	override def isEmpty = false
}

/**
 * If method [is] called and there is no value in box then method [lazyValue]
 * will be called to initialize box value.
 */
class LazyVarBox[V](lazyValue: () => V) extends CannotBeEmptyVarBox[V] {

	override def is: V = {
		if (_isEmpty) {
			valueBox = Full(lazyValue())
		}
		_is
	}
}

/**
 * If method [is] called and there is no value in box then method [anywayValue]
 * will be called to get result.
 */
class AnywayVarBox[V](defaultValue: () => V) extends CannotBeEmptyVarBox[V] {

	override def is: V = {
		if (_isEmpty) defaultValue()
		else _is
	}
}

class EmptyVarBoxException(msg: String) extends NullPointerException(msg)