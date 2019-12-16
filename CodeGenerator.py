'''
 *   @author Nguyen Hua Phung
 *   @version 1.0
 *   23/10/2015
 *   This file provides a simple version of code generator
 *
'''
from Utils import *
from StaticCheck import *
from StaticError import *
from Emitter import Emitter
from Frame import Frame
from abc import ABC, abstractmethod
from functools import reduce

class CodeGenerator(Utils):
    def __init__(self):
        self.libName = "io"

    def init(self):
        return [Symbol("getInt", MType(list(), IntType()), CName(self.libName)),
                    Symbol("putInt", MType([IntType()], VoidType()), CName(self.libName)),
                    Symbol("putIntLn", MType([IntType()], VoidType()), CName(self.libName)),
                    Symbol("putFloatLn", MType([FloatType()], VoidType()), CName(self.libName)),
                    Symbol("getFloat", MType(list(), FloatType()), CName(self.libName)),
                    Symbol("putFloat", MType([FloatType()], VoidType()), CName(self.libName)),
                    Symbol("putBool", MType([BoolType()], VoidType()), CName(self.libName)),
                    Symbol("putBoolLn", MType([BoolType()], VoidType()), CName(self.libName)),
                    Symbol("putString", MType([StringType()], VoidType()), CName(self.libName)),
                    Symbol("putStringLn", MType([StringType()], VoidType()), CName(self.libName)),
                    Symbol("putLn", MType(list(), VoidType()), CName(self.libName))
                ]

    def gen(self, ast, dir_):
        #ast: AST
        #dir_: String

        gl = self.init()
        gc = CodeGenVisitor(ast, gl, dir_)
        gc.visit(ast, None)

class ClassType(Type):
    def __init__(self, cname):
        #cname: String
        self.cname = cname

    def __str__(self):
        return "ClassType"

    def accept(self, v, param):
        return v.visitClassType(self, param)

class SubBody():
    def __init__(self, frame, sym):
        #frame: Frame
        #sym: List[Symbol]

        self.frame = frame
        self.sym = sym

class Access():
    def __init__(self, frame, sym, isLeft, isFirst):
        #frame: Frame
        #sym: List[Symbol]
        #isLeft: Boolean
        #isFirst: Boolean

        self.frame = frame
        self.sym = sym
        self.isLeft = isLeft
        self.isFirst = isFirst

class Val(ABC):
    pass

class Index(Val):
    def __init__(self, value):
        #value: Int

        self.value = value

class CName(Val):
    def __init__(self, value):
        #value: String

        self.value = value

class CodeGenVisitor(BaseVisitor, Utils):
    def __init__(self, astTree, env, dir_):
        #astTree: AST
        #env: List[Symbol]
        #dir_: File

        self.astTree = astTree
        self.env = env
        self.className = "MCClass"
        self.path = dir_
        self.emit = Emitter(self.path + "/" + self.className + ".j")

    def visitProgram(self, ast, c):
        #ast: Program
        #c: Any

        self.emit.printout(self.emit.emitPROLOG(self.className, "java.lang.Object"))
        declList = self.env
        # e = SubBody(None, self.env)
        # for x in ast.decl:
        #     e = self.visit(x, e)
        for x in ast.decl:
            if type(x) is FuncDecl:
                declList = [Symbol(x.name.name, MType([y.varType for y in x.param], x.returnType),
                                     CName(self.className))] + declList
            else:
                symbol = self.visit(x, (SubBody(None, None), True))
                declList = [symbol] + declList

        e = SubBody(None, declList)
        [self.visit(x, e) for x in ast.decl if type(x) is FuncDecl]
        # generate default constructor
        self.genMETHOD(FuncDecl(Id("<init>"), list(), None, Block(list())), c, Frame("<init>", VoidType))
        self.emit.emitEPILOG()
        return c

    def genMETHOD(self, consdecl, o, frame):
        #consdecl: FuncDecl
        #o: Any
        #frame: Frame
        glenv = o
        isInit = consdecl.returnType is None
        isMain = consdecl.name.name == "main" and len(consdecl.param) == 0 and type(consdecl.returnType) is VoidType
        returnType = VoidType() if isInit else consdecl.returnType
        methodName = "<init>" if isInit else consdecl.name.name
        intype = [ArrayPointerType(StringType())] if isMain else list()
        mtype = MType(intype, returnType)

        self.emit.printout(self.emit.emitMETHOD(methodName, mtype, not isInit, frame))

        frame.enterScope(True)

        # Generate code for parameter declarations
        if isInit:
            self.emit.printout(self.emit.emitVAR(frame.getNewIndex(), "this", ClassType(self.className),
                                                 frame.getStartLabel(), frame.getEndLabel(), frame))
        if isMain:
            self.emit.printout(self.emit.emitVAR(frame.getNewIndex(), "args", ArrayPointerType(StringType()),
                                                 frame.getStartLabel(), frame.getEndLabel(), frame))

        var_List = SubBody(frame, glenv)
        for x in consdecl.param:
            var_List = self.visit(x, (var_List, False))
        for x in list(filter(lambda decl: type(decl) is VarDecl, consdecl.body.member)):
            var_List = self.visit(x, (var_List, False))
        # body = consdecl.body

        self.emit.printout(self.emit.emitLABEL(frame.getStartLabel(), frame))

        for x in list(filter(lambda decl: type(decl) is not VarDecl, consdecl.body.member)):
            self.visit(x, var_List)
        # Generate code for statements
        if isInit:
            self.emit.printout(self.emit.emitREADVAR("this", ClassType(self.className), 0, frame))
            self.emit.printout(self.emit.emitINVOKESPECIAL(frame))
        # list(map(lambda x: self.visit(x, SubBody(frame, glenv)), body.member))

        self.emit.printout(self.emit.emitLABEL(frame.getEndLabel(), frame))
        if type(returnType) is VoidType:
            self.emit.printout(self.emit.emitRETURN(VoidType(), frame))
        self.emit.printout(self.emit.emitENDMETHOD(frame))
        frame.exitScope()

    def visitFuncDecl(self, ast, o):
        #ast: FuncDecl
        #o: Any

        subctxt = o
        frame = Frame(ast.name, ast.returnType)
        self.genMETHOD(ast, subctxt.sym, frame)
        return SubBody(None, [Symbol(ast.name, MType(list(), ast.returnType), CName(self.className))] + subctxt.sym)

    def visitVarDecl(self, ast, o):
        ctxt, isGlobal = o
        frame = ctxt.frame
        varName = ast.variable
        varType = ast.varType
        if isGlobal:
            self.emit.printout(self.emit.emitATTRIBUTE(varName, varType, False, ""))
            return Symbol(ast.variable, ast.varType)
        idx = frame.getNewIndex()
        self.emit.printout(self.emit.emitVAR(idx, varName, varType, frame.getStartLabel(),
                                             frame.getEndLabel(), frame))
        return SubBody(frame, [Symbol(varName, varType, Index(idx))] + ctxt.sym)

    # Visit statements:
    def visitBlock(self, ast, o):
        ctxt = o
        frame = o.frame
        nenv = o.sym
        var_List = SubBody(frame, nenv)
        frame.enterScope(False)
        for x in list(filter(lambda stmt: type(stmt) is VarDecl, ast.member)):
            var_List = self.visit(x, (var_List, False))
        self.emit.printout(self.emit.emitLABEL(frame.getStartLabel(), frame))
        for x in list(filter(lambda decl: type(decl) is not VarDecl, ast.member)):
            self.visit(x, var_List)
        self.emit.printout(self.emit.emitLABEL(frame.getEndLabel(), frame))
        frame.exitScope()

    def visitIf(self, ast, o):
        ctxt = o
        frame = ctxt.frame
        nenv = ctxt.sym
        expCode, expType = self.visit(ast.expr, Access(frame, nenv, False, True))
        self.emit.printout(expCode)
        labelThen = frame.getNewLabel()  # eval is true
        labelExit = frame.getNewLabel()  # label end
        if ast.elseStmt is None:
            self.emit.printout(self.emit.emitIFTRUE(labelThen, frame))
            self.emit.printout(self.emit.emitGOTO(labelExit, frame))
            self.emit.printout(self.emit.emitLABEL(labelThen, frame))
            # hasReturnStmt = True in [self.visit(x, o) for x in [ast.thenStmt]] and hasReturnStmt
            # if type(ast.thenStmt) is Block:
            #     [self.visit(x, o) for x in ast.thenStmt.member]
            # else:
            self.visit(ast.thenStmt, o)
        else:  # has else stmt
            self.emit.printout(self.emit.emitIFTRUE(labelThen, frame))
            if type(ast.thenStmt) is Block:
                [self.visit(x, o) for x in ast.elseStmt.member]
            else:
                self.visit(ast.elseStmt, o)
            self.emit.printout(self.emit.emitGOTO(labelExit, frame))
            self.emit.printout(self.emit.emitLABEL(labelThen, frame))
            # if type(ast.thenStmt) is Block:
            #     [self.visit(x, o) for x in ast.thenStmt.member]
            # else:
            self.visit(ast.thenStmt, o)
        self.emit.printout(self.emit.emitLABEL(labelExit, frame))

    def visitDowhile(self, ast, o):
        ctxt = o
        frame = ctxt.frame
        nenv = ctxt.sym
        labelLoop = frame.getNewLabel()
        labelExit = frame.getNewLabel()
        frame.enterLoop()
        self.emit.printout(self.emit.emitLABEL(labelLoop, frame))
        [self.visit(x, o) for x in ast.sl]
        self.emit.printout(self.emit.emitLABEL(frame.getContinueLabel(), frame))
        expCode, expType = self.visit(ast.exp, Access(frame, nenv, False, True))
        self.emit.printout(expCode)
        self.emit.printout(self.emit.emitIFTRUE(labelLoop, frame))
        self.emit.printout(self.emit.emitGOTO(labelExit, frame))
        self.emit.printout(self.emit.emitLABEL(labelExit, frame))
        self.emit.printout(self.emit.emitLABEL(frame.getBreakLabel(), frame))
        frame.exitLoop()

    def visitFor(self, ast, o):
        ctxt = o
        frame = ctxt.frame
        nenv = ctxt.sym
        labelLoop = frame.getNewLabel()
        labelExit = frame.getNewLabel()
        # exp1Code, exp1Type = self.visit(ast.expr1,  o)
        exp2Code, _ = self.visit(ast.expr2, Access(frame, nenv, False, True))
        # exp3Code, _ = self.visit(ast.expr3, o)
        frame.enterLoop()
        # self.emit.printout(exp1Code)
        self.visit(ast.expr1, o)
        self.emit.printout(self.emit.emitLABEL(labelLoop, frame))
        self.emit.printout(exp2Code)
        self.emit.printout(self.emit.emitIFFALSE(labelExit, frame))
        self.visit(ast.loop, o)
        # self.emit.printout(exp3Code)
        self.emit.printout(self.emit.emitLABEL(frame.getContinueLabel(), frame))
        self.visit(ast.expr3, o)
        self.emit.printout(self.emit.emitGOTO(labelLoop, frame))
        self.emit.printout(self.emit.emitLABEL(labelExit, frame))
        self.emit.printout(self.emit.emitLABEL(frame.getBreakLabel(), frame))
        frame.exitLoop()

    def visitBreak(self, ast, o):
        ctxt = o
        frame = ctxt.frame
        self.emit.printout(self.emit.emitGOTO(frame.getBreakLabel(), frame))

    def visitContinue(self, ast, o):
        ctxt = o
        frame = ctxt.frame
        self.emit.printout(self.emit.emitGOTO(frame.getContinueLabel(), frame))

    # Visit expression
    def visitBinaryOp(self, ast, o):
        ctxt = o
        frame = ctxt.frame
        op = ast.op
        nenv = ctxt.sym
        if op == "=":
            expCode, expType = self.visit(ast.right, Access(frame, nenv, False, True))
            lhsCode, lhsType = self.visit(ast.left, Access(frame, nenv, True, True))
            if type(lhsType) is FloatType and type(expType) is IntType:
                expCode = expCode + self.emit.emitI2F(frame)
            # self.emit.printout(expCode + lhsCode)
            returnCode = expCode + lhsCode
            frame.push()
            if type(o) is SubBody:
                # return "", lhsType
                self.emit.printout(returnCode)
            else:
                returnCode = expCode + self.emit.emitDUP(frame) + lhsCode
                return returnCode, lhsType
        else:
            leftCode, leftType = self.visit(ast.left, Access(frame, nenv, False, True))
            rightCode, rightType = self.visit(ast.right, Access(frame, nenv, False, True))
            expr_type = FloatType() if type(leftType) is not type(rightType) else leftType
            if type(expr_type) is FloatType:
                if type(leftType) is IntType:
                    leftCode = leftCode + self.emit.emitI2F(frame)
                if type(rightType) is IntType:
                    rightCode = rightCode + self.emit.emitI2F(frame)
            if op in ["+", "-"]:
                return leftCode + rightCode + self.emit.emitADDOP(op, expr_type, frame), expr_type
            elif op in ["*", "/"]:
                return leftCode + rightCode + self.emit.emitMULOP(op, expr_type, frame), expr_type
            elif op == "%":
                return leftCode + rightCode + self.emit.emitMOD(frame), IntType()
            elif op == "||":
                return leftCode + rightCode + self.emit.emitOROP(frame), BoolType()
            elif op == "&&":
                return leftCode + rightCode + self.emit.emitANDOP(frame), BoolType()
            else:
                return leftCode + rightCode + self.emit.emitREOP(op, expr_type, frame), BoolType()

    def visitUnaryOp(self, ast, o):
        ctxt = o
        frame = ctxt.frame
        op = ast.op
        expCode, expType = self.visit(ast.body, o)
        if op == "!":
            return expCode + self.emit.emitNOT(BoolType(), frame), BoolType()
        else:
            return expCode + self.emit.emitNEGOP(expType, frame), expType

    def visitCallExpr(self, ast, o):
        #ast: CallExpr
        #o: Any
        ctxt = o
        frame = ctxt.frame
        nenv = ctxt.sym
        sym = self.lookup(ast.method.name, nenv, lambda x: x.name)
        cname = sym.value.value
    
        ctype = sym.mtype

        in_ = ("", list())
        for x in ast.param:
            str1, typ1 = self.visit(x, Access(frame, nenv, False, True))
            in_ = (in_[0] + str1, in_[1].append(typ1))
        self.emit.printout(in_[0])
        self.emit.printout(self.emit.emitINVOKESTATIC(cname + "/" + ast.method.name, ctype, frame))

    def visitId(self, ast, o):
        ctxt = o
        frame = ctxt.frame
        symbols = ctxt.sym
        isLeft = ctxt.isLeft
        isFirst = ctxt.isFirst
        sym = self.lookup(ast.name, symbols, lambda x: x.name)
        # recover status of stack in frame
        if not isFirst and isLeft:
            frame.push()
        elif not isFirst and not isLeft:
            frame.pop()
        emitType = sym.mtype
        if sym.value is None:  # not index -> global var - static field
            if isLeft:
                retCode = self.emit.emitPUTSTATIC(self.className + "/" + sym.name, emitType, frame)
            else:
                retCode = self.emit.emitGETSTATIC(self.className + "/" + sym.name, emitType, frame)
        else:
            if isLeft:
                retCode = self.emit.emitWRITEVAR(sym.name, emitType, sym.value.value, frame)
            else:
                retCode = self.emit.emitREADVAR(sym.name, emitType, sym.value.value, frame)
        return retCode, sym.mtype

    def visitIntLiteral(self, ast, o):
        #ast: IntLiteral
        #o: Any

        ctxt = o
        frame = ctxt.frame
        return self.emit.emitPUSHICONST(ast.value, frame), IntType()

    def visitFloatLiteral(self, ast, o):
        #ast: FloatLiteral
        #o: Any

        ctxt = o
        frame = ctxt.frame
        return self.emit.emitPUSHFCONST(str(ast.value), frame), FloatType()

    def visitStringLiteral(self, ast, o):
        ctxt = o
        frame = ctxt.frame
        return self.emit.emitPUSHCONST('''"''' + str(ast.value) + '''"''', StringType(), frame), StringType()

    def visitBooleanLiteral(self, ast, o):
        ctxt = o
        frame = ctxt.frame
        return self.emit.emitPUSHICONST(str(ast.value).lower(), frame), BoolType()
