package com.aionescu.tli.parser;

import java.util.function.BiFunction;
import java.util.function.BinaryOperator;
import java.util.function.Function;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.Field;
import com.aionescu.tli.ast.expr.*;
import com.aionescu.tli.ast.expr.kind.ExprKind;
import com.aionescu.tli.ast.expr.kind.ExprKind.L;
import com.aionescu.tli.ast.expr.kind.ExprKind.R;
import com.aionescu.tli.ast.expr.Expr;
import com.aionescu.tli.ast.stmt.*;
import com.aionescu.tli.ast.type.TBool;
import com.aionescu.tli.ast.type.TFun;
import com.aionescu.tli.ast.type.TInt;
import com.aionescu.tli.ast.type.TRec;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.utils.collections.list.List;
import com.aionescu.tli.utils.collections.map.Map;
import com.aionescu.tli.utils.control.Maybe;
import com.aionescu.tli.utils.uparsec.Parser;

import static com.aionescu.tli.utils.uparsec.Parser.*;

import com.aionescu.tli.utils.Pair;
import com.aionescu.tli.utils.Unit;

public final class TLParser {
  private static final Parser<Stmt> _parser;
  private static final Parser<Integer> _number;
  private static final Parser<Ident> _ident;
  private static final Parser<Unit> _ws;
  private static final Parser<Unit> _comma;
  private static final Parser<Unit> _colon;
  private static final Parser<Unit> _arrow;
  private static final Parser<Expr<R>> _expr;
  private static final Parser<Expr<R>> _exprNoWith;

  static {
    var multiLineFwdRef = Parser.<Unit>fwdRef();
    var multiLine = multiLineFwdRef.fst;

    var shebang = string("#!")._and(anyChar.manyTill(eof.or(newline.skip()))).skip();

    var multiLine_ = string("{-")._and(multiLine.or(anyChar.skip()).manyTill(string("-}"))).skip();
    multiLineFwdRef.snd.set(multiLine_);

    var singleLine = string("--")._and(anyChar.manyTill(eof.or(newline.skip()))).skip();

    var comment = singleLine.or(multiLine);
    _ws = spaces._and(comment._and(spaces).many()).skip();

    _comma = ch(',')._and(_ws);
    _colon = _ws.and_(ch(':'))._and(_ws);
    _arrow = _ws.and_(string("<-"))._and(_ws);

    Parser<Function<Integer, Integer>> sign = ch('-').map_(i -> -i);
    _number = digit.many1().map(List::asString).map(Integer::parseInt);
    Parser<Expr<R>> int_ = ap(sign.option(i -> i), _number).map(IntLit::new);

    Parser<Expr<R>> bool_ = choice(
      string("True").map_(true),
      string("False").map_(false)
    ).map(BoolLit::new);

    var simpleLit = int_.or(bool_);

    var reserved = List.of("if", "else", "while", "and", "or");
    Function<Ident, Parser<Ident>> notReserved =
      i -> reserved.find(e -> e.equals(i.name)).match(() -> Parser.pure(i), a -> Parser.fail());

    var fstChar = lower;
    var sndChar = letter.or(digit).or(ch('\''));

    _ident = liftA2(List::cons, fstChar, sndChar.many()).and_(_ws).map(List::asString).map(Ident::new).bind(notReserved);

    var primType = choice(
      string("Int").map_(TInt.t),
      string("Bool").map_(TBool.t));

    var typeFwdRef = Parser.<Type>fwdRef();
    var type = typeFwdRef.fst;

    Parser<Type> ttup = _tuple(a -> new TRec<>(Field.fTup, _tupToRec(a)), type);
    Parser<Type> trec = _record('{', _ident, _colon, type).map(a -> new TRec<>(Field.fRec, a));

    var typeNoFun = choice(trec, ttup, primType);

    var type_ = typeNoFun.chainr1(_ws._and(string("->"))._and(_ws).map_(TFun::new));
    typeFwdRef.snd.set(type_);

    var exprFwdRef = Parser.<Expr<R>>fwdRef();
    _expr = exprFwdRef.fst;

    Parser<Expr<R>> vtup = _tuple(a -> new RecLit<>(Field.fTup, _tupToRec(a)), _expr);
    Parser<Expr<R>> vrec = _record('{', _ident, _arrow, _expr).map(a -> new RecLit<>(Field.fRec, a));

    var lvalue = _member(TLParser.<L>_var()).or(_var());

    var opMul = Parser.<BinaryOperator<Expr<R>>>choice(
      ch('*').map_((a, b) -> new Arith(a, Arith.Op.MUL, b)),
      ch('/').map_((a, b) -> new Arith(a, Arith.Op.DIV, b)),
      ch('%').map_((a, b) -> new Arith(a, Arith.Op.REM, b)),
      ch('&').map_(RecUnion::new)
    ).and_(_ws);

    var opAdd = Parser.<BinaryOperator<Expr<R>>>choice(
      ch('+').map_((a, b) -> new Arith(a, Arith.Op.ADD, b)),
      ch('-').map_((a, b) -> new Arith(a, Arith.Op.SUB, b))
    ).and_(_ws);

    var opComp = Parser.<BinaryOperator<Expr<R>>>choice(
      string("<=").map_((a, b) -> new Comp(a, Comp.Op.LTE, b)),
      string(">=").map_((a, b) -> new Comp(a, Comp.Op.GTE, b)),
      string("<>").map_((a, b) -> new Comp(a, Comp.Op.NEQ, b)),
      ch('<').map_((a, b) -> new Comp(a, Comp.Op.LT, b)),
      ch('>').map_((a, b) -> new Comp(a, Comp.Op.GT, b)),
      ch('=').map_((a, b) -> new Comp(a, Comp.Op.EQ, b))
    ).and_(_ws);

    var opLogic = Parser.<BinaryOperator<Expr<R>>>choice(
      string("and").map_((a, b) -> new Logic(a, Logic.Op.AND, b)),
      string("or").map_((a, b) -> new Logic(a, Logic.Op.OR, b))
    ).and_(_ws);

    var lamParam = _parens('(', ')', liftA2(Pair::new, _ident.and_(_colon), type));
    var lam = ch('\\')._and(liftA2(TLParser::_mkLam, lamParam.many1().and_(ch('.')).and_(_ws), _expr));

    var exprNoMember = choice(lam, vrec, vtup, simpleLit, _var()).and_(_ws);
    _exprNoWith = _member(exprNoMember).or(exprNoMember);

    var withRecord = _withExpr((a, b) -> new RecWith<>(a, Field.fRec, b), _ident);
    var withTup = _withExpr((a, b) -> new RecWith<>(a, Field.fTup, b), _number);

    var exprNoOps = withRecord.or(withTup).or(_exprNoWith);

    Parser<Expr<R>> termMul = exprNoOps.chainl1(_ws.map_(App::new));

    var termAdd = termMul.chainl1(opMul);
    var termComp = termAdd.chainl1(opAdd);
    var termLogic = termComp.chainl1(opComp);

    var termFinal = termLogic.chainr1(opLogic);
    exprFwdRef.snd.set(termFinal);

    Parser<Stmt> print = string("print").and_(_ws)._and(_expr).map(Print::new);

    var colon = _ws._and(ch(':'))._and(_ws);
    Parser<Stmt> decl = liftA2(Decl::new, _ident.and_(colon), type);

    var arrow = _ws._and(string("<-"))._and(_ws);
    Parser<Stmt> assign = liftA2(Assign::new, lvalue.and_(arrow), _expr);

    var typeOrInfer = ch('_').map_(Maybe.<Type>nothing()).or(type.map(Maybe::just));
    Parser<Stmt> declAssign = liftA3(DeclAssign::new, _ident.and_(colon), typeOrInfer.and_(arrow), _expr);

    var stmtFwdRef = Parser.<Stmt>fwdRef();
    var stmt = stmtFwdRef.fst;

    var block = ch('{')._and(_ws)._and(stmt).and_(_ws).and_(ch('}')).and_(_ws);

    var ifCond = string("if")._and(_ws)._and(_expr).and_(_ws);
    var elseBlock = string("else")._and(_ws)._and(block).option(Nop.nop);

    Parser<Stmt> if_ = liftA3(If::new, ifCond, block, elseBlock);

    var whileCond = string("while")._and(_ws)._and(_expr).and_(_ws);
    Parser<Stmt> while_ = liftA2(While::new, whileCond, block);

    var stmt_ = choice(while_, if_, declAssign, assign, decl, print).and_(_ws);
    var compound = stmt_.chainr1(ch(';').and_(_ws).map_(Compound::new)).option(Nop.nop);
    stmtFwdRef.snd.set(compound);

    _parser = shebang.option(Unit.UNIT)._and(_ws)._and(stmt).and_(eof);
  }

  private static <K extends ExprKind> Parser<Expr<K>> _var() {
    return _ident.map(Var::new);
  }

  private static <A> Parser<A> _parens(char begin, char end, Parser<A> p) {
    return p.between(ch(begin).and_(_ws), ch(end).and_(_ws));
  }

  private static <A> Parser<A> _tuple(Function<List<A>, A> ctor, Parser<A> term) {
    var elems = term.sepBy(_comma).and(_comma.map_(true).or(_ws.map_(false)));

    Function<Pair<List<A>, Boolean>, A> _mkTup = p ->
      p.fst.length() == 1 && !p.snd
      ? p.fst.uncons().unwrap().fst
      : ctor.apply(p.fst);

    return _parens('(', ')', elems.map(_mkTup));
  }

  private static <I extends Comparable<I>, A> Parser<Map<I, A>> _record(char begin, Parser<I> idx, Parser<Unit> sep, Parser<A> rhs) {
    var term = liftA2(Pair::new, idx.and_(_ws).and_(sep).and_(_ws), rhs.and_(_ws));
    var elems = term.sepBy(_comma).and_(_comma.or(_ws));

    Function<List<Pair<I, A>>, Parser<List<Pair<I, A>>>> unique = es ->
      es.map(Pair::fst_).allUnique()
      ? pure(es)
      : fail();

    return _parens(begin, '}', elems).bind(unique).map(Map::fromList);
  }

  private static <A> Map<Integer, A> _tupToRec(List<A> as) {
    return Map.fromList(as.indexed());
  }

  private static <K extends ExprKind> Expr<K> _unroll(Expr<K> lhs, Object idx) {
    return
      idx instanceof Ident
      ? new RecMember<>(lhs, Field.fRec, (Ident)idx)
      : new RecMember<>(lhs, Field.fTup, (Integer)idx);
  }

  private static <K extends ExprKind> Parser<Expr<K>> _member(Parser<Expr<K>> lhs) {
    var idxs = ch('.')._and(_number.map(a -> (Object)a).or(_ident.map(a -> (Object)a))).and_(_ws).many();
    return liftA2((a, b) -> b.foldl(TLParser::_unroll, a), lhs, idxs);
  }

  private static Expr<R> _mkLam(List<Pair<Ident, Type>> l, Expr<R> e) {
    return l.match(
      () -> e,
      (a, as) -> new Lam(a.fst, a.snd, _mkLam(as, e)));
  }

  private static <I extends Comparable<I>> Parser<Expr<R>> _withExpr(BiFunction<Expr<R>, Map<I, Expr<R>>, Expr<R>> ctor, Parser<I> idx) {
    return liftA2(ctor, ch('{')._and(_ws)._and(_exprNoWith).and_(_ws), _record('|', idx, _arrow, _expr));
  }

  public static Stmt parse(String code) {
    return _parser.parse(code);
  }
}
