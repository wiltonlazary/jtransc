package com.jtransc.types

import com.jtransc.ast.*
import com.jtransc.ds.cast
import com.jtransc.error.deprecated
import com.jtransc.error.invalidOp
import com.jtransc.error.noImpl
import com.jtransc.input.isStatic
import org.objectweb.asm.Handle
import org.objectweb.asm.Opcodes
import org.objectweb.asm.Type
import org.objectweb.asm.tree.*
import java.util.*

val Handle.ast: AstMethodRef get() = AstMethodRef(FqName.fromInternal(this.owner), this.name, AstType.demangleMethod(this.desc))

//const val DEBUG = true
const val DEBUG = false

fun Asm2Baf(clazz: AstType.REF, method: MethodNode): BafBody {
	//val DEBUG = method.name == "paramOrderSimple"
	if (DEBUG) {
		println("--------------------------------------------------------------------")
		println("::::::::::::: ${clazz.name}.${method.name}:${method.desc}")
		println("--------------------------------------------------------------------")
	}

	val tryCatchBlocks = method.tryCatchBlocks.cast<TryCatchBlockNode>()
	val basicBlocks = BasicBlocks(clazz, method, DEBUG)
	val locals = basicBlocks.locals
	val labels = basicBlocks.labels

	for (b in tryCatchBlocks) {
		labels.ref(labels.label(b.start))
		labels.ref(labels.label(b.end))
		labels.ref(labels.label(b.handler))
		labels.referencedHandlers += b.handler
	}

	val prefix = createFunctionPrefix(clazz, method, locals)
	basicBlocks.queue(method.instructions.first, prefix)

	for (b in tryCatchBlocks) {
		val prefix = ArrayList<Baf>()
		val catchStack = Stack<BafLocal>()
		val errorLocal = locals.local(AstType.OBJECT, 0)
		prefix.add(Baf.CAUGHT_EXCEPTION(errorLocal))
		catchStack.push(errorLocal)
		basicBlocks.queue(b.handler, BasicBlock.Input(catchStack, prefix))
	}

	//val writtenBasicBlocks = hashSetOf<BasicBlock>()

	val body2 = method.instructions.toArray().toList().flatMap {
		//println(basicBlocks.getBasicBlockForLabel(it))
		basicBlocks.getBasicBlockForLabel(it)?.stms?.toList() ?: listOf()
	}.filter {
		if (it is Baf.LABEL) {
			it.label.refs >= 1
		} else {
			true
		}
	}

	return BafBody(
		body2,
		locals,
		tryCatchBlocks.map {
			BafTrap(
				start = labels.label(it.start),
				end = labels.label(it.end),
				handler = labels.label(it.handler),
				exception = if (it.type != null) AstType.REF_INT2(it.type) else AstType.OBJECT
			)
		}
	)
}

class BasicBlocks(
	private val clazz: AstType.REF,
	private val method: MethodNode,
	private val DEBUG: Boolean
) {
	val locals = BafLocals()
	val labels = Labels()
	private val blocks = hashMapOf<AbstractInsnNode, BasicBlock>()

	fun queue(entry: AbstractInsnNode, input: BasicBlock.Input) {
		if (entry in blocks) return
		val bb = BasicBlockBuilder(clazz, method, locals, labels, input.prefix.clone() as ArrayList<Baf>, DEBUG).call(entry, input)
		blocks[bb.entry] = bb
		for (item in bb.outgoingAll) queue(item, bb.output)
	}

	fun getBasicBlockForLabel(label: AbstractInsnNode): BasicBlock? {
		return blocks[label]
	}
}

fun createFunctionPrefix(clazz: AstType.REF, method: MethodNode, locals: BafLocals): BasicBlock.Input {
	//val localsOutput = arrayListOf<AstExpr.LocalExpr>()
	val stms = ArrayList<Baf>()
	val methodType = AstType.demangleMethod(method.desc)

	var idx = 0
	if (!method.isStatic()) {
		val type = AstType.REF(clazz.name)
		stms.add(Baf.THIS(locals.local(type, idx)))
		idx++
	}

	for (arg in methodType.args) {
		val type = arg.type
		stms.add(Baf.PARAM(locals.local(type, idx), arg))
		idx++
	}

	return BasicBlock.Input(Stack(), stms)
}

data class BasicBlock(
	val input: Input,
	val output: Input,
	val entry: AbstractInsnNode,
	val stms: ArrayList<Baf>,
	val next: AbstractInsnNode?,
	val outgoing: List<AbstractInsnNode>
) {
	val outgoingAll = (if (next != null) listOf(next) else listOf()) + outgoing

	data class Input(
		val stack: Stack<BafLocal>,
		//val locals: Map<BafLocalId, BafLocal>,
		val prefix: ArrayList<Baf> = ArrayList<Baf>()
	)
}

class Labels {
	var lastId = 0
	val labels = hashMapOf<AbstractInsnNode, BafLabel>()
	val referencedLabels = hashSetOf<BafLabel>()
	val referencedHandlers = hashSetOf<AbstractInsnNode>()

	fun label(label: AbstractInsnNode): BafLabel {
		if (label !in labels) {
			labels[label] = BafLabel("label_$lastId")
			lastId++
		}
		return labels[label]!!
	}

	fun ref(label: AbstractInsnNode): BafLabel {
		return ref(label(label))
	}

	fun ref(label: BafLabel): BafLabel {
		referencedLabels += label
		label.ref()
		return label
	}
}

/*
fun localPair(index: Int, type: AstType, prefix: String) = Locals.ID(index, fixType(type), prefix)

class Locals {
	data class ID(val index: Int, val type: AstType, val prefix: String)

	var tempLocalId = 0
	val locals = hashMapOf<Locals.ID, AstExpr.LocalExpr>()  // @TODO: remove this

	fun local(type: AstType, index: Int, prefix: String = "l"): AstExpr.LocalExpr {
		val info = localPair(index, type, prefix)
		//if (info !in locals) locals[info] = AstExpr.LOCAL(AstLocal(index, "local${index}_$type", type))
		val type2 = fixType(type)
		if (info !in locals) locals[info] = AstExpr.LOCAL(AstLocal(index, "$prefix${nameType(type2)}${index}", type2))
		return locals[info]!!
	}

	fun tempLocal(type: AstType): AstExpr.LocalExpr {
		return local(type, tempLocalId++, "t")
	}
}
*/

class BafLocals {
	data class ID(val index: Int, val type: AstType, val kind: BafLocal.Kind)

	private val locals = hashMapOf<ID, BafLocal>()

	fun getAllLocals(): List<BafLocal> = locals.values.toList()

	var tempID = 0

	fun _alloc(index:Int, type:AstType, kind: BafLocal.Kind): BafLocal {
		val type2 = if (type is AstType.Primitive) type else AstType.OBJECT
		val idd = ID(index, type2, kind)
		if (idd !in locals) locals[idd] = BafLocal(index, type2, kind)
		return locals[idd]!!
	}

	fun temp(type: AstType): BafLocal {
		return _alloc(tempID++, type, BafLocal.Kind.TEMP)
	}

	fun local(type: AstType, index: Int): BafLocal {
		return _alloc(index, type, BafLocal.Kind.LOCAL)
	}

	fun frameVar(type: AstType, index: Int): BafLocal {
		return _alloc(index, type, BafLocal.Kind.FRAME)
	}
}

// http://stackoverflow.com/questions/4324321/java-local-variables-how-do-i-get-a-variable-name-or-type-using-its-index
private class BasicBlockBuilder(
	val clazz: AstType.REF,
	val method: MethodNode,
	val locals: BafLocals,
	val labels: Labels,
	val _bafStms: ArrayList<Baf> = ArrayList<Baf>(),
	val DEBUG: Boolean
) {
	companion object {
		val PTYPES = listOf(AstType.INT, AstType.LONG, AstType.FLOAT, AstType.DOUBLE, AstType.OBJECT, AstType.BYTE, AstType.CHAR, AstType.SHORT)
		val CTYPES = listOf(AstBinop.EQ, AstBinop.NE, AstBinop.LT, AstBinop.GE, AstBinop.GT, AstBinop.LE, AstBinop.EQ, AstBinop.NE)
	}

	val methodType = AstType.demangleMethod(method.desc)


	val _bafStack = Stack<BafLocal>()
	var lastLine = -1

	private fun add(baf: Baf) {
		for (ref in baf.readReferences) ref.addRead(baf)
		_bafStms.add(baf)
	}

	private fun push(baf: Baf.RESULT) {
		add(baf)
		baf.target.addWrite(baf)
		push(baf.target)
	}

	private fun push(local: BafLocal) {
		_bafStack.push(local)
	}

	private fun pop(): BafLocal {
		if (_bafStack.isEmpty()) {
			println("empty stack!")
		}
		return _bafStack.pop()
	}

	private fun peek(): BafLocal {
		if (_bafStack.isEmpty()) {
			println("empty stack!")
		}
		return _bafStack.peek()
	}

	private fun peekType(): AstType {
		return peek().type
	}

	fun handleField(i: FieldInsnNode) {
		val ref = AstFieldRef(AstType.REF_INT2(i.owner).fqname.fqname, i.name, com.jtransc.ast.AstType.demangle(i.desc))
		when (i.opcode) {
			Opcodes.GETSTATIC -> {
				push(Baf.FIELD_STATIC_GET(locals.temp(ref.type), ref))
			}
			Opcodes.GETFIELD -> {
				val obj = pop()
				push(Baf.FIELD_INSTANCE_GET(locals.temp(ref.type), ref, obj))
			}
			Opcodes.PUTSTATIC -> {
				val param = pop()
				add(Baf.FIELD_STATIC_SET(ref, param))
			}
			Opcodes.PUTFIELD -> {
				val param = pop()
				val obj = pop()
				add(Baf.FIELD_INSTANCE_SET(ref, obj, param))
			}
			else -> invalidOp
		}
	}

	//  peephole optimizations

	fun optimize(e: AstExpr.BINOP): AstExpr {
		return e
	}

	fun cast(expr: AstExpr, to: AstType) = AstExprUtils.cast(expr, to)
	fun fastcast(expr: AstExpr, to: AstType) = AstExprUtils.fastcast(expr, to)

	fun pushBinop(type: AstType, op: AstBinop) {
		val r = pop()
		val l = pop()
		push(Baf.BINOP(locals.temp(type), l, op, r))
	}

	fun pushUnop(type: AstType, op: AstUnop) {
		val r = pop()
		push(Baf.UNOP(locals.temp(type), op, r))
	}

	private fun pushCast(type: AstType) {
		val r = pop()
		push(Baf.CAST(locals.temp(type), r))
	}

	fun arrayLoad(elementType: AstType): Unit {
		val bafIndex = pop()
		val bafArray = pop()
		val target = locals.temp(elementType)
		add(Baf.ARRAY_LOAD(target, bafArray, bafIndex))
		push(target)
	}

	fun arrayStore(elementType: AstType): Unit {
		val bafExpr = pop()
		val bafIndex = pop()
		val bafArray = pop()
		add(Baf.ARRAY_STORE(bafArray, elementType, bafIndex, bafExpr))
	}

	private var stackPopToLocalsItemsCount = 0

	fun popDouble(): List<BafLocal> = if (peekType().isLongOrDouble()) popCount(1) else popCount(2)
	fun popSingle(): List<BafLocal> = popCount(1)
	fun popCount(count: Int): List<BafLocal> = (0 until count).map { pop() }

	fun pushList(list: List<BafLocal>) {
		for (i in list) push(i)
	}

	private fun pushLiteral(value: Any?) {
		push(Baf.IMMEDIATE(locals.temp(AstType.fromConstant(value)), value))
	}

	fun handleInsn(i: InsnNode): Unit {
		val op = i.opcode
		when (i.opcode) {
			Opcodes.NOP -> Unit
			Opcodes.ACONST_NULL -> pushLiteral(null)
			in Opcodes.ICONST_M1..Opcodes.ICONST_5 -> pushLiteral((op - Opcodes.ICONST_0).toInt())
			in Opcodes.LCONST_0..Opcodes.LCONST_1 -> pushLiteral((op - Opcodes.LCONST_0).toLong())
			in Opcodes.FCONST_0..Opcodes.FCONST_2 -> pushLiteral((op - Opcodes.FCONST_0).toFloat())
			in Opcodes.DCONST_0..Opcodes.DCONST_1 -> pushLiteral((op - Opcodes.DCONST_0).toDouble())
			in Opcodes.IALOAD..Opcodes.SALOAD -> arrayLoad(PTYPES[op - Opcodes.IALOAD])
			in Opcodes.IASTORE..Opcodes.SASTORE -> arrayStore(PTYPES[op - Opcodes.IASTORE])
			Opcodes.POP -> {
				// We store it, so we don't lose all the calculated stuff!
				popSingle()
			}
			Opcodes.POP2 -> {
				popDouble()
			}
			Opcodes.DUP -> {
				val v1 = popSingle()
				pushList(v1)
				pushList(v1)
			}
			Opcodes.DUP2 -> {
				val v1 = popDouble()
				pushList(v1)
				pushList(v1)
			}
			Opcodes.DUP_X1 -> {
				//untestedWarn2("DUP_X1")
				val v1 = popSingle()
				val v2 = popSingle()
				pushList(v1)
				pushList(v2)
				pushList(v1)
			}
			Opcodes.DUP_X2 -> {
				val v1 = popSingle()
				val v2 = popDouble()
				pushList(v1)
				pushList(v2)
				pushList(v1)
			}
			Opcodes.DUP2_X1 -> {
				//untestedWarn2("DUP2_X1")
				val v1 = popDouble()
				val v2 = popSingle()
				pushList(v1)
				pushList(v2)
				pushList(v1)
			}
			Opcodes.DUP2_X2 -> {
				//untestedWarn2("DUP2_X2")
				val v1 = popDouble()
				val v2 = popDouble()
				pushList(v1)
				pushList(v2)
				pushList(v1)
			}
			Opcodes.SWAP -> {
				val v1 = pop()
				val v2 = pop()
				push(v1)
				push(v2)
			}
			in Opcodes.INEG..Opcodes.DNEG -> pushUnop(PTYPES[op - Opcodes.INEG], AstUnop.NEG)

			in Opcodes.IADD..Opcodes.DADD -> pushBinop(PTYPES[op - Opcodes.IADD], AstBinop.ADD)
			in Opcodes.ISUB..Opcodes.DSUB -> pushBinop(PTYPES[op - Opcodes.ISUB], AstBinop.SUB)
			in Opcodes.IMUL..Opcodes.DMUL -> pushBinop(PTYPES[op - Opcodes.IMUL], AstBinop.MUL)
			in Opcodes.IDIV..Opcodes.DDIV -> pushBinop(PTYPES[op - Opcodes.IDIV], AstBinop.DIV)
			in Opcodes.IREM..Opcodes.DREM -> pushBinop(PTYPES[op - Opcodes.IREM], AstBinop.REM)
			in Opcodes.ISHL..Opcodes.LSHL -> pushBinop(PTYPES[op - Opcodes.ISHL], AstBinop.SHL)
			in Opcodes.ISHR..Opcodes.LSHR -> pushBinop(PTYPES[op - Opcodes.ISHR], AstBinop.SHR)
			in Opcodes.IUSHR..Opcodes.LUSHR -> pushBinop(PTYPES[op - Opcodes.IUSHR], AstBinop.USHR)
			in Opcodes.IAND..Opcodes.LAND -> pushBinop(PTYPES[op - Opcodes.IAND], AstBinop.AND)
			in Opcodes.IOR..Opcodes.LOR -> pushBinop(PTYPES[op - Opcodes.IOR], AstBinop.OR)
			in Opcodes.IXOR..Opcodes.LXOR -> pushBinop(PTYPES[op - Opcodes.IXOR], AstBinop.XOR)

			Opcodes.I2L, Opcodes.F2L, Opcodes.D2L -> pushCast(AstType.LONG)
			Opcodes.I2F, Opcodes.L2F, Opcodes.D2F -> pushCast(AstType.FLOAT)
			Opcodes.I2D, Opcodes.L2D, Opcodes.F2D -> pushCast(AstType.DOUBLE)
			Opcodes.L2I, Opcodes.F2I, Opcodes.D2I -> pushCast(AstType.INT)
			Opcodes.I2B -> pushCast(AstType.BYTE)
			Opcodes.I2C -> pushCast(AstType.CHAR)
			Opcodes.I2S -> pushCast(AstType.SHORT)

			Opcodes.LCMP -> pushBinop(AstType.LONG, AstBinop.LCMP)
			Opcodes.FCMPL -> pushBinop(AstType.FLOAT, AstBinop.CMPL)
			Opcodes.FCMPG -> pushBinop(AstType.FLOAT, AstBinop.CMPG)
			Opcodes.DCMPL -> pushBinop(AstType.DOUBLE, AstBinop.CMPL)
			Opcodes.DCMPG -> pushBinop(AstType.DOUBLE, AstBinop.CMPG)

			Opcodes.ARRAYLENGTH -> push(Baf.ARRAY_LENGTH(locals.temp(AstType.INT), pop()))
			Opcodes.MONITORENTER -> add(Baf.MONITOR_ENTER(pop()))
			Opcodes.MONITOREXIT -> add(Baf.MONITOR_EXIT(pop()))
			else -> invalidOp("$op")
		}
	}

	fun handleMultiArray(i: MultiANewArrayInsnNode) {
		when (i.opcode) {
			Opcodes.MULTIANEWARRAY -> {
				val arrayType = AstType.REF_INT(i.desc) as AstType.ARRAY
				push(Baf.NEW_ARRAY(locals.temp(arrayType), arrayType, (0 until i.dims).map { pop() }.reversed()))
			}
			else -> invalidOp("$i")
		}
	}

	fun handleType(i: TypeInsnNode) {
		val type = AstType.REF_INT(i.desc)
		when (i.opcode) {
			Opcodes.NEW -> push(Baf.NEW(locals.temp(type as AstType.REF), type))
			Opcodes.ANEWARRAY -> push(Baf.NEW_ARRAY(locals.temp(type.array), type.array, listOf(pop())))
			Opcodes.CHECKCAST -> push(Baf.CHECK_CAST(locals.temp(type), pop(), type))
			Opcodes.INSTANCEOF -> push(Baf.INSTANCE_OF(locals.temp(AstType.BOOL), pop(), type))
			else -> invalidOp("$i")
		}
	}

	private fun pushCopy(l: BafLocal, r: BafLocal) {
		if (l != r) {
			push(Baf.COPY(l, r))
		} else {
			push(r)
		}
	}

	private fun addCopy(l: BafLocal, r: BafLocal) {
		if (l != r) {
			add(Baf.COPY(l, r))
		}
	}

	fun handleVar(i: VarInsnNode) {
		val op = i.opcode
		val index = i.`var`

		fun load(type: AstType) {
			pushCopy(locals.temp(type), locals.local(type, index))
		}

		fun store(type: AstType) {
			addCopy(locals.local(type, index), pop())
		}

		when (op) {
			in Opcodes.ILOAD..Opcodes.ALOAD -> load(PTYPES[op - Opcodes.ILOAD])
			in Opcodes.ISTORE..Opcodes.ASTORE -> store(PTYPES[op - Opcodes.ISTORE])
			Opcodes.RET -> deprecated
			else -> invalidOp
		}
	}

	fun addJump(cond: BafLocal?, trueLabel: BafLabel, falseLabel: BafLabel?) {
		if (DEBUG) println("Preserve because jump")
		labels.ref(trueLabel)
		if (cond != null) {
			add(Baf.IF_GOTO(cond, trueLabel, falseLabel!!))
		} else {
			add(Baf.GOTO(trueLabel))
		}
	}


	fun handleLdc(i: LdcInsnNode) {
		val cst = i.cst
		when (cst) {
			is Int, is Float, is Long, is Double, is String -> pushLiteral(cst)
			is Type -> pushLiteral(AstType.REF_INT(cst.internalName))
			else -> invalidOp
		}
	}

	fun handleInt(i: IntInsnNode) {
		when (i.opcode) {
			Opcodes.BIPUSH -> pushLiteral(i.operand.toByte())
			Opcodes.SIPUSH -> pushLiteral(i.operand.toShort())
			Opcodes.NEWARRAY -> {
				val type = when (i.operand) {
					Opcodes.T_BOOLEAN -> AstType.BOOL
					Opcodes.T_CHAR -> AstType.CHAR
					Opcodes.T_FLOAT -> AstType.FLOAT
					Opcodes.T_DOUBLE -> AstType.DOUBLE
					Opcodes.T_BYTE -> AstType.BYTE
					Opcodes.T_SHORT -> AstType.SHORT
					Opcodes.T_INT -> AstType.INT
					Opcodes.T_LONG -> AstType.LONG
					else -> invalidOp
				}
				push(Baf.NEW_ARRAY(locals.temp(AstType.ARRAY(type, 1)), type.array, listOf(pop())))
			}
			else -> invalidOp
		}
	}

	fun handleMethod(i: MethodInsnNode) {
		val type = AstType.REF_INT(i.owner)
		val clazz = if (type is AstType.REF) type else AstType.OBJECT
		val methodRef = com.jtransc.ast.AstMethodRef(clazz.fqname.fqname, i.name, AstType.demangleMethod(i.desc))
		val isSpecial = i.opcode == Opcodes.INVOKESPECIAL

		val args = methodRef.type.args.reversed().map { pop() }.reversed()
		val obj = if (i.opcode != Opcodes.INVOKESTATIC) pop() else null

		val info = Baf.INVOKEINFO(clazz, obj, methodRef, args, isSpecial)

		if (methodRef.type.retVoid) {
			add(Baf.INVOKE_VOID(info))
		} else {
			push(Baf.INVOKE(locals.temp(methodRef.type.ret), info))
		}
	}

	fun handleInvokeDynamic(i: InvokeDynamicInsnNode) {
		val bsm = i.bsm.ast
		val info = Baf.INVOKEDYNAMICINFO(
			AstMethodWithoutClassRef(i.name, AstType.demangleMethod(i.desc)),
			bsm,
			i.bsmArgs.map {
				when (it) {
					is org.objectweb.asm.Type -> when (it.sort) {
						Type.METHOD -> AstType.demangleMethod(it.descriptor)
						else -> noImpl("${it.sort} : ${it}")
					}
					is org.objectweb.asm.Handle -> {
						val kind = AstMethodHandle.Kind.fromId(it.tag)
						val type = AstType.demangleMethod(it.desc)
						AstMethodHandle(type, AstMethodRef(FqName.fromInternal(it.owner), it.name, type), kind)
					}
					else -> it
				}
			}
		)
		push(Baf.INVOKEDYNAMIC(locals.temp(bsm.type.ret), info))
	}

	fun handleIinc(i: IincInsnNode) {
		add(Baf.IINCR(locals.local(AstType.INT, i.`var`), i.incr))
	}

	fun handleLineNumber(i: LineNumberNode) {
		lastLine = i.line
		add(Baf.LINE(i.line))
	}

	fun call(entry: AbstractInsnNode, input: BasicBlock.Input): BasicBlock {
		var i: AbstractInsnNode? = entry
		var next: AbstractInsnNode? = null
		val outgoing = arrayListOf<AbstractInsnNode>()

		// RESTORE INPUTS
		if (DEBUG && input.stack.size >= 2) println("---------")

		add(Baf.LABEL(labels.label(entry)))

		if (i is LabelNode) {
			//add(Baf.LABEL(labels.label(i)))
			i = i.next
		}

		this._bafStack.clear()
		for (i in input.stack.clone() as Stack<BafLocal>) {
			this._bafStack += i
		}

		if (DEBUG) {
			println("**** BASIC_BLOCK ${clazz.name}.${method.name}:${method.desc} :: BASIC_BLOCK: $entry, $input")
		}

		loop@while (i != null) {
			if (DEBUG) println(AsmOpcode.disasm(i))
			val op = i.opcode
			when (i) {
				is FieldInsnNode -> handleField(i)
				is InsnNode -> {
					when (op) {
						in Opcodes.IRETURN..Opcodes.ARETURN -> {
							add(Baf.RETURN(pop(), methodType.ret))
							next = null
							break@loop
						}
						Opcodes.RETURN -> {
							add(Baf.RETURN_VOID())
							next = null
							break@loop
						}
						Opcodes.ATHROW -> {
							add(Baf.THROW(pop()))
							next = null
							break@loop
						}
						else -> handleInsn(i)
					}
				}
				is JumpInsnNode -> {
					when (op) {
						in Opcodes.IFEQ..Opcodes.IFLE -> {
							val l = pop()
							push(Baf.IMMEDIATE(locals.temp(AstType.INT), 0))
							val r = pop()
							push(Baf.BINOP(locals.temp(AstType.BOOL), l, CTYPES[op - Opcodes.IFEQ], r))
							val condition = pop()
							persistStack()
							addJump(condition, labels.ref(i.label), labels.ref(i.next))
						}
						in Opcodes.IFNULL..Opcodes.IFNONNULL -> {
							val l = pop()
							push(Baf.IMMEDIATE(locals.temp(AstType.NULL), null))
							val r = pop()
							push(Baf.BINOP(locals.temp(AstType.BOOL), l, CTYPES[op - Opcodes.IFNULL], r))
							val condition = pop()
							persistStack()
							addJump(condition, labels.ref(i.label), labels.ref(i.next))
						}
						in Opcodes.IF_ICMPEQ..Opcodes.IF_ACMPNE -> {
							val r = pop()
							val l = pop()
							push(Baf.BINOP(locals.temp(AstType.BOOL), l, CTYPES[op - Opcodes.IF_ICMPEQ], r))
							val condition = pop()
							persistStack()
							addJump(condition, labels.ref(i.label), labels.ref(i.next))
						}
						Opcodes.GOTO -> {
							persistStack()
							addJump(null, labels.ref(i.label), null)
						}
						Opcodes.JSR -> deprecated
						else -> invalidOp
					}

					if (op == Opcodes.GOTO) {
						next = i.label
					} else {
						next = i.next
						outgoing.add(i.label)
					}
					break@loop
				}
				is LookupSwitchInsnNode -> {
					persistStack()
					val labels2 = i.labels.cast<LabelNode>()
					add(Baf.SWITCH_GOTO(
						pop(),
						labels.ref(labels.label(i.dflt)),
						i.keys.cast<Int>().zip(labels2.map { labels.ref(labels.label(it)) })
					))
					next = i.dflt
					outgoing.addAll(labels2)
					break@loop
				}
				is TableSwitchInsnNode -> {
					persistStack()
					val labels2 = i.labels.cast<LabelNode>()
					add(Baf.SWITCH_GOTO(
						pop(),
						labels.ref(labels.label(i.dflt)),
						(i.min..i.max).zip(labels2.map { labels.ref(labels.label(it)) })
					))
					next = i.dflt
					outgoing.addAll(labels2)
					break@loop
				}
				is LabelNode -> {
					if (DEBUG) println("Preserve because label")
					next = i
					addJump(null, labels.label(i), null)
					break@loop
				}
				is FrameNode -> Unit
				is LdcInsnNode -> handleLdc(i)
				is IntInsnNode -> handleInt(i)
				is MethodInsnNode -> handleMethod(i)
				is TypeInsnNode -> handleType(i)
				is VarInsnNode -> handleVar(i)
				is InvokeDynamicInsnNode -> handleInvokeDynamic(i)
				is IincInsnNode -> handleIinc(i)
				is LineNumberNode -> handleLineNumber(i)
				is MultiANewArrayInsnNode -> handleMultiArray(i)
				else -> invalidOp("$i")
			}
			i = i.next
		}

		//dumpExprs()

		return BasicBlock(
			input = input,
			output = BasicBlock.Input(_bafStack.clone() as Stack<BafLocal>),
			entry = entry,
			stms = _bafStms,
			next = next,
			outgoing = outgoing
		)
	}

	private fun persistStack() {
		val stack2 = _bafStack.toList().withIndex()
		_bafStack.clear()
		for ((index, item) in stack2) {
			pushCopy(locals.frameVar(item.type, index), item)
		}
	}
}
