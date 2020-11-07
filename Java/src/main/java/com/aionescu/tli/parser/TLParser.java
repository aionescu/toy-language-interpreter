package com.aionescu.tli.parser;

import java.util.function.BinaryOperator;
import java.util.function.Function;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.expr.*;
import com.aionescu.tli.ast.expr.kind.ExprKind.R;
import com.aionescu.tli.ast.stmt.*;
import com.aionescu.tli.ast.type.TBool;
import com.aionescu.tli.ast.type.TInt;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.utils.collections.list.List;
import com.aionescu.tli.utils.control.Maybe;
import com.aionescu.tli.utils.uparsec.Parser;
import static com.aionescu.tli.utils.uparsec.Parser.*;
import com.aionescu.tli.utils.Unit;

public final class TLParser {
  private static Parser<Stmt> _mkParser() {
    var multiLineFwdRef = Parser.<Unit>fwdRef();
    var multiLine = multiLineFwdRef.fst;

    var shebang = string("#!")._and(anyChar.manyTill(eof.or(newline.skip()))).skip();

    var multiLine_ = string("{-")._and(multiLine.or(anyChar.skip()).manyTill(string("-}"))).skip();
    multiLineFwdRef.snd.set(multiLine_);

    var singleLine = string("--")._and(anyChar.manyTill(eof.or(newline.skip()))).skip();

    var comment = singleLine.or(multiLine);
    var ws = spaces._and(comment._and(spaces).many()).skip();

    Parser<Function<Integer, Integer>> sign = ch('-').map_(i -> -i);
    var number = digit.many1().map(List::asString).map(Integer::parseInt);
    Parser<Expr<R>> int_ = ap(sign.option(i -> i), number).map(IntLit::new);

    Parser<Expr<R>> bool_ = choice(
      string("True").map_(true),
      string("False").map_(false)
    ).map(BoolLit::new);

    var lit = int_.or(bool_);

    var fstChar = lower;
    var sndChar = letter.or(digit).or(ch('\''));

    var ident = liftA2(List::cons, fstChar, sndChar.many()).and_(ws).map(List::asString).map(Ident::new);

    var type = choice(
      string("Int").map_(TInt.t),
      string("Bool").map_(TBool.t));

    var opMul = Parser.<BinaryOperator<Expr<R>>>choice(
      ch('*').map_((a, b) -> new Arith(a, Arith.Op.MUL, b)),
      ch('/').map_((a, b) -> new Arith(a, Arith.Op.DIV, b)),
      ch('%').map_((a, b) -> new Arith(a, Arith.Op.REM, b))
    ).and_(ws);

    var opAdd = Parser.<BinaryOperator<Expr<R>>>choice(
      ch('+').map_((a, b) -> new Arith(a, Arith.Op.ADD, b)),
      ch('-').map_((a, b) -> new Arith(a, Arith.Op.SUB, b))
    ).and_(ws);

    var opComp = Parser.<BinaryOperator<Expr<R>>>choice(
      string("<=").map_((a, b) -> new Comp(a, Comp.Op.LTE, b)),
      string(">=").map_((a, b) -> new Comp(a, Comp.Op.GTE, b)),
      string("<>").map_((a, b) -> new Comp(a, Comp.Op.NEQ, b)),
      ch('<').map_((a, b) -> new Comp(a, Comp.Op.LT, b)),
      ch('>').map_((a, b) -> new Comp(a, Comp.Op.GT, b)),
      ch('=').map_((a, b) -> new Comp(a, Comp.Op.EQ, b))
    ).and_(ws);

    var opLogic = Parser.<BinaryOperator<Expr<R>>>choice(
      string("and").map_((a, b) -> new Logic(a, Logic.Op.AND, b)),
      string("or").map_((a, b) -> new Logic(a, Logic.Op.OR, b))
    ).and_(ws);

    var exprFwdRef = Parser.<Expr<R>>fwdRef();
    var expr = exprFwdRef.fst;

    Parser<Expr<R>> termMul = choice(
      expr.between(ch('(').and_(ws), ch(')').and(ws)),
      lit,
      ident.map(Var::new)
    ).and_(ws);

    var termAdd = termMul.chainl1(opMul);
    var termComp = termAdd.chainl1(opAdd);
    var termLogic = termComp.chainl1(opComp);

    var termFinal = termLogic.chainr1(opLogic);
    exprFwdRef.snd.set(termFinal);

    Parser<Stmt> print = string("print").and_(ws)._and(expr).map(Print::new);

    var colon = ws._and(ch(':'))._and(ws);
    Parser<Stmt> decl = liftA2(Decl::new, ident.and_(colon), type);

    var arrow = ws._and(string("<-"))._and(ws);
    Parser<Stmt> assign = liftA2(Assign::new, ident.and_(arrow).map(Var::new), expr);

    var typeOrInfer = ch('_').map_(Maybe.<Type>nothing()).or(type.map(Maybe::just));
    Parser<Stmt> declAssign = liftA3(DeclAssign::new, ident.and_(colon), typeOrInfer.and_(arrow), expr);

    var stmtFwdRef = Parser.<Stmt>fwdRef();
    var stmt = stmtFwdRef.fst;

    var block = ch('{')._and(ws)._and(stmt).and_(ws).and_(ch('}')).and_(ws);

    var ifCond = string("if")._and(ws)._and(expr).and_(ws);
    var elseBlock = string("else")._and(ws)._and(block).option(Nop.nop);

    Parser<Stmt> if_ = liftA3(If::new, ifCond, block, elseBlock);

    var whileCond = string("while")._and(ws)._and(expr).and_(ws);
    Parser<Stmt> while_ = liftA2(While::new, whileCond, block);

    var stmt_ = choice(while_, if_, declAssign, assign, decl, print).and_(ws);
    var compound = stmt_.chainr1(ch(';').and_(ws).map_(Compound::new)).option(Nop.nop);
    stmtFwdRef.snd.set(compound);

    var program = shebang.option(Unit.UNIT)._and(ws)._and(stmt).and_(eof);
    return program;
  }

  private static final Parser<Stmt> _parser = _mkParser();

  public static Stmt parse(String code) {
    return _parser.parse(code);
  }
}
