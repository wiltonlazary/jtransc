package com.jtransc.types

import com.jtransc.ast.*
import com.jtransc.ds.stripNulls


data class BafLabel(val name: String) {
	val ast = AstLabel(name)
	var refs = 0

	fun ref() {
		refs++
	}
}

data class BafLocal(val index: Int, val type: AstType, val kind: Kind) {
	enum class Kind { TEMP, LOCAL, FRAME }

	val local = AstLocal(index, "${kind}_${type.primch}_$index", type)
	val expr = AstExpr.LOCAL(local)
	val writeNodes = arrayListOf<Baf>()
	val readNodes = arrayListOf<Baf>()

	fun addWrite(node: Baf) {
		writeNodes.add(node)
	}

	fun addRead(node: Baf) {
		readNodes.add(node)
	}
}

class BafBody(val stms: List<Baf>, val locals: BafLocals, val traps: List<BafTrap>) {
}


class BafTrap(val start: BafLabel, val end: BafLabel, val handler: BafLabel, val exception: AstType.REF) {
}

class BafList() : Iterable<Baf> {
	override fun iterator(): Iterator<Baf> {
		return object : Iterator<Baf> {
			var next = first

			override fun hasNext(): Boolean = next != null

			override fun next(): Baf {
				val out = next!!
				next = next!!.next
				return out
			}
		}
	}

	var first: Baf? = null; private set
	var last: Baf? = null; private set


	constructor(base: Iterable<Baf>) : this() {
		for (i in base) add(i)
	}

	fun <T : Baf> add(item: T): T {
		if (last == null) {
			first = item
			last = item
		} else {
			last!!.next = item
			item.prev = last!!
			last = item
		}
		return item
	}
}

sealed class Baf {
	var prev: Baf? = null
	var next: Baf? = null
	open val writeReferences: List<BafLocal> = listOf()
	open val readReferences: List<BafLocal> = listOf()

	abstract class RESULT : Baf() {
		abstract val target: BafLocal
		override val writeReferences: List<BafLocal> by lazy { listOf(target) }
	}

	class IMMEDIATE(override val target: BafLocal, val value: Any?) : RESULT() {
	}

	class PARAM(override val target: BafLocal, val argument: AstArgument) : RESULT() {
	}

	class THIS(override val target: BafLocal) : RESULT() {
	}

	class COPY(override val target: BafLocal, val right: BafLocal) : RESULT() {
		override val readReferences = listOf(right)
	}

	class UNOP(override val target: BafLocal, val op: AstUnop, val right: BafLocal) : RESULT() {
		override val readReferences = listOf(right)
	}

	class CAST(override val target: BafLocal, val right: BafLocal) : RESULT() {
		override val readReferences = listOf(right)
	}

	class BINOP(override val target: BafLocal, val left: BafLocal, val op: AstBinop, val right: BafLocal) : RESULT() {
		override val readReferences = listOf(left, right)
		val lt = left.type
		val rt = right.type
		val commonType: AstType = if (lt == AstType.LONG || rt == AstType.LONG) {
			AstType.LONG
		} else if (lt == AstType.BOOL || rt == AstType.BOOL) {
			AstType.INT
		} else {
			lt
		}
		val isShift = (op == AstBinop.SHL || op == AstBinop.SHR || op == AstBinop.USHR)
		val ltt = if (isShift) lt else commonType
		val rtt = if (isShift) AstType.INT else commonType
	}

	class ARRAY_LOAD(override val target: BafLocal, val array: BafLocal, val index: BafLocal) : RESULT() {
		override val readReferences = listOf(array, index)
	}

	class FIELD_STATIC_GET(override val target: BafLocal, val field: AstFieldRef) : RESULT() {
	}

	class ARRAY_STORE(val array: BafLocal, val elementType: AstType, val index: BafLocal, val expr: BafLocal) : Baf() {
		override val readReferences = listOf<BafLocal>(array, index, expr)
	}

	class FIELD_INSTANCE_GET(override val target: BafLocal, val field: AstFieldRef, val instance: BafLocal) : RESULT() {
		override val readReferences = listOf<BafLocal>(instance)
	}

	class FIELD_STATIC_SET(val field: AstFieldRef, val value: BafLocal) : Baf() {
		override val readReferences = listOf<BafLocal>(value)
	}

	class FIELD_INSTANCE_SET(val field: AstFieldRef, val instance: BafLocal, val value: BafLocal) : Baf() {
		override val readReferences = listOf<BafLocal>(instance, value)
	}

	class MONITOR_ENTER(val instance: BafLocal) : Baf() {
		override val readReferences = listOf<BafLocal>(instance)
	}

	class MONITOR_EXIT(val instance: BafLocal) : Baf() {
		override val readReferences = listOf<BafLocal>(instance)
	}

	class ARRAY_LENGTH(override val target: BafLocal, val array: BafLocal) : RESULT() {
		override val readReferences = listOf<BafLocal>(array)
	}

	class NEW(override val target: BafLocal, val type: AstType.REF) : RESULT() {
	}

	class NEW_ARRAY(override val target: BafLocal, val arrayType: AstType.ARRAY, val lengths: List<BafLocal>) : RESULT() {
		override val readReferences = listOf<BafLocal>() + lengths
	}

	class LINE(val line: Int) : Baf() {
	}

	class RETURN(val retval: BafLocal, val rettype: AstType) : Baf() {
		override val readReferences = listOf(retval)
	}

	class RETURN_VOID() : Baf() {
	}

	class THROW(val value: BafLocal) : Baf() {
		override val readReferences = listOf(value)
	}

	/*
	class LOCAL_LOAD(override val target: BafLocal, val index: Int) : RESULT() {
	}

	class LOCAL_STORE(val index: Int, val right: BafLocal) : Baf() {
	}
	*/

	class SWITCH_GOTO(val subject: BafLocal, val defaultLabel: BafLabel, val labels: List<Pair<Int, BafLabel>>) : Baf() {

	}

	class GOTO(val label: BafLabel) : Baf() {
	}

	class IF_GOTO(val cond: BafLocal, val trueLabel: BafLabel, val falseLabel: BafLabel) : Baf() {
		override val readReferences = listOf(cond)
	}

	class CHECK_CAST(override val target: BafLocal, val instance: BafLocal, val type: AstType) : RESULT() {
		override val readReferences = listOf(instance)
	}

	class INSTANCE_OF(override val target: BafLocal, val instance: BafLocal, val checkType: AstType) : RESULT() {
		override val readReferences = listOf(instance)
	}

	class IINCR(val local: BafLocal, val incr: Int) : Baf() {
		override val writeReferences: List<BafLocal> = listOf(local)
		override val readReferences: List<BafLocal> = listOf(local)
	}

	class INVOKEINFO(val clazz: AstType.REF, val instance: BafLocal?, val methodRef: AstMethodRef, val args: List<BafLocal>, val isSpecial: Boolean) {
		val readReferences = (listOf(instance).stripNulls() + args)
	}

	interface INVOKELIKE {
		val info: INVOKEINFO
	}

	class INVOKEDYNAMICINFO(val astMethodWithoutClassRef: AstMethodWithoutClassRef, val ast: AstMethodRef, val map: List<Any?>) {
		val readReferences = listOf<BafLocal>()
	}

	class INVOKE_VOID(override val info: INVOKEINFO) : Baf(), INVOKELIKE {
		override val readReferences = info.readReferences
	}

	class INVOKE(override val target: BafLocal, override val info: INVOKEINFO) : RESULT(), INVOKELIKE {
		override val readReferences = info.readReferences
	}

	class INVOKEDYNAMIC_VOID(override val target: BafLocal, val info: INVOKEDYNAMICINFO) : RESULT() {
		override val readReferences = info.readReferences
	}

	class INVOKEDYNAMIC(override val target: BafLocal, val info: INVOKEDYNAMICINFO) : RESULT() {
		override val readReferences = info.readReferences
	}

	class LABEL(val label: BafLabel) : Baf() {
	}

	class CAUGHT_EXCEPTION(override val target: BafLocal) : RESULT() {

	}
}

//val AstLocal.expr: AstExpr.LOCAL get() = AstExpr.LOCAL(this)

//fun BafLocal.toAst(): AstExpr.LValueExpr {
//	return AstExpr.LOCAL(this.local)
//}

