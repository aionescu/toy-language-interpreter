package com.aionescu.tli.parser;

import java.math.BigInteger;
import java.util.function.BiFunction;
import java.util.function.BinaryOperator;
import java.util.function.Function;

import com.aionescu.tli.ast.*;
import com.aionescu.tli.ast.Field.*;
import com.aionescu.tli.ast.expr.*;
import com.aionescu.tli.ast.stmt.*;
import com.aionescu.tli.ast.type.*;
import com.aionescu.tli.ast.val.*;
import com.aionescu.tli.exn.eval.IncorrectFileFormatException;
import com.aionescu.tli.utils.Pair;
import com.aionescu.tli.utils.Unit;
import com.aionescu.tli.utils.control.Maybe;
import com.aionescu.tli.utils.data.list.List;
import com.aionescu.tli.utils.data.map.Map;
import com.aionescu.tli.utils.uparsec.Parser;

import static com.aionescu.tli.utils.uparsec.Parser.*;

public final class TLParser {
  private static final Parser<Stmt> _parser;
  private static final Parser<Field> _recField;
  private static final Parser<Field> _tupField;
  private static final Parser<Expr> _var;
  private static final Parser<Unit> _ws;
  private static final Parser<Unit> _comma;
  private static final Parser<Unit> _colon;
  private static final Parser<Unit> _equals;
  private static final Parser<Expr> _expr;
  private static final Parser<Expr> _exprNoWith;

  private static final Parser<Val> _val;
  private static final Parser<Val> _valLine;

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
    _equals = _ws.and_(ch('='))._and(_ws);

    Parser<Function<BigInteger, BigInteger>> sign = ch('-').map_(BigInteger::negate);
    var number = digit.many1().map(List::asString);
    _tupField = number.map(Integer::parseInt).map(TupField::new);
    Parser<Expr> int_ = ap(sign.option(i -> i), number.map(BigInteger::new)).map(IntLit::new);

    Parser<Boolean> boolRaw = choice(
      string("True").map_(true),
      string("False").map_(false)
    );

    Parser<Expr> bool_ = boolRaw.map(BoolLit::new);

    var escaped = ch('\\')._and(oneOf("\\\"0nrvtbf")).map(TLParser::_unescape);
    var regular = noneOf("\\\"\0\n\r\t\b\f");
    var chr = regular.or(escaped);
    var quote = ch('"');

    Parser<String> strRaw = chr.many().between(quote, quote).map(List::asString);
    Parser<Expr> str = strRaw.map(StrLit::new);

    var simpleLit = choice(str, int_, bool_);

    var reserved = List.of("if", "else", "while", "and", "or", "default", "let", "not", "repeat", "until");
    Function<Ident, Parser<Ident>> notReserved =
      i -> reserved.find(e -> e.equals(i.name)).match(() -> Parser.pure(i), a -> Parser.fail());

    var fstChar = lower;
    var sndChar = letter.or(digit).or(ch('\''));

    var ident = liftA2(List::cons, fstChar, sndChar.many()).and_(_ws).map(List::asString).map(Ident::new).bind(notReserved);
    _recField = ident.map(RecField::new);

    var primType = choice(
      string("Int").map_(TInt.t),
      string("Bool").map_(TBool.t),
      string("Str").map_(TStr.t));

    var typeFwdRef = Parser.<Type>fwdRef();
    var type = typeFwdRef.fst;

    Parser<Type> ttup = _tuple(a -> new TRec(false, _tupToRec(a)), type);
    Parser<Type> trec = _record('{', _recField, _colon, type).map(a -> new TRec(true, a));

    var typeNoFunFwdRef = Parser.<Type>fwdRef();
    var typeNoFun = typeNoFunFwdRef.fst;

    Parser<Type> tref = ch('&')._and(_ws)._and(typeNoFun).map(TRef::new);

    var typeNoFun_ = choice(tref, trec, ttup, primType);
    typeNoFunFwdRef.snd.set(typeNoFun_);

    var type_ = typeNoFun.chainr1(_ws._and(string("->"))._and(_ws).map_(TFun::new));
    typeFwdRef.snd.set(type_);

    var exprFwdRef = Parser.<Expr>fwdRef();
    _expr = exprFwdRef.fst;

    Parser<Expr> vtup = _tuple(a -> new RecLit(false, _tupToRec(a)), _expr);
    Parser<Expr> vrec = _record('{', _recField, _equals, _expr).map(a -> new RecLit(true, a));

    _var = ident.map(Var::new);

    var lvalue = _member(_var).or(_var);

    var opMul = Parser.<BinaryOperator<Expr>>choice(
      ch('*').map_((a, b) -> new Arith(a, Arith.Op.MUL, b)),
      ch('/').map_((a, b) -> new Arith(a, Arith.Op.DIV, b)),
      ch('%').map_((a, b) -> new Arith(a, Arith.Op.REM, b)),
      ch('&').map_(RecUnion::new)
    ).and_(_ws);

    var opAdd = Parser.<BinaryOperator<Expr>>choice(
      ch('+').map_((a, b) -> new Arith(a, Arith.Op.ADD, b)),
      ch('-').map_((a, b) -> new Arith(a, Arith.Op.SUB, b))
    ).and_(_ws);

    var opComp = Parser.<BinaryOperator<Expr>>choice(
      string("<=").map_((a, b) -> new Comp(a, Comp.Op.LTE, b)),
      string(">=").map_((a, b) -> new Comp(a, Comp.Op.GTE, b)),
      string("==").map_((a, b) -> new Comp(a, Comp.Op.EQ, b)),
      string("!=").map_((a, b) -> new Comp(a, Comp.Op.NEQ, b)),
      ch('<').map_((a, b) -> new Comp(a, Comp.Op.LT, b)),
      ch('>').map_((a, b) -> new Comp(a, Comp.Op.GT, b))
    ).and_(_ws);

    var opLogic = Parser.<BinaryOperator<Expr>>choice(
      string("and").map_((a, b) -> new Logic(a, Logic.Op.AND, b)),
      string("or").map_((a, b) -> new Logic(a, Logic.Op.OR, b))
    ).and_(_ws);

    var lamParam = _parens('(', ')', liftA2(Pair::new, ident.and_(_colon), type));
    var lam = liftA2(TLParser::_mkLam, lamParam.many1().and_(string("->")).and_(_ws), _expr);

    Parser<Expr> default_ = string("default")._and(_ws)._and(type).map(Default::new);
    var exprNoOpsFwdRef = Parser.<Expr>fwdRef();
    var exprNoOps = exprNoOpsFwdRef.fst;

    Parser<Expr> deref = ch('!')._and(_ws)._and(exprNoOps).map(Deref::new);
    Parser<Expr> not = string("not")._and(_ws)._and(exprNoOps).map(Not::new);

    var exprNoMember = choice(deref, default_, not, lam, vrec, vtup, simpleLit, _var).and_(_ws);
    _exprNoWith = _member(exprNoMember).or(exprNoMember);

    var withRecord = _withExpr((a, b) -> new RecWith(a, true, b), _recField);
    var withTup = _withExpr((a, b) -> new RecWith(a, false, b), _tupField);

    var exprNoOps_ = withRecord.or(withTup).or(_exprNoWith);
    exprNoOpsFwdRef.snd.set(exprNoOps_);

    Parser<Expr> termMul = exprNoOps.chainl1(_ws.map_(App::new));

    var termAdd = termMul.chainl1(opMul);
    var termComp = termAdd.chainl1(opAdd);
    var termLogic = termComp.chainl1(opComp);

    var termFinal = termLogic.chainr1(opLogic);
    exprFwdRef.snd.set(termFinal);

    Parser<Stmt> print = string("print").and_(_ws)._and(_expr).map(Print::new);

    Parser<Stmt> decl = string("let")._and(_ws)._and(liftA2(Decl::new, ident.and_(_colon), type));

    Parser<Stmt> assign = liftA2(Assign::new, lvalue.and_(_equals), _expr);

    var typeOrInfer = _colon._and(type.map(Maybe::just)).option(Maybe.nothing());
    Parser<Stmt> declAssign = string("let")._and(_ws)._and(liftA3(DeclAssign::new, ident, typeOrInfer, _equals._and(_expr)));

    var stmtFwdRef = Parser.<Stmt>fwdRef();
    var stmt = stmtFwdRef.fst;

    var block = ch('{')._and(_ws)._and(stmt).and_(_ws).and_(ch('}')).and_(_ws);

    var ifCond = string("if")._and(_ws)._and(_expr).and_(_ws);
    var elseBlock = string("else")._and(_ws)._and(block).option(Nop.nop);

    Parser<Stmt> if_ = liftA3(If::new, ifCond, block, elseBlock);

    var whileCond = string("while")._and(_ws)._and(_expr).and_(_ws);
    Parser<Stmt> while_ = liftA2(While::new, whileCond, block);

    var repeatHeader = string("repeat")._and(_ws)._and(block).and_(string("until")._and(_ws));
    Parser<Stmt> repeatUntil = Parser.liftA2(RepeatUntil::new, repeatHeader, _expr);

    Parser<Stmt> open = string("open")._and(_ws)._and(_expr).map(Open::new);
    Parser<Stmt> read = Parser.liftA3(Read::new, ident, _colon._and(type), _equals._and(string("read")._and(_ws)._and(_expr)));
    Parser<Stmt> close = string("close")._and(_ws)._and(_expr).map(Close::new);

    Parser<Stmt> new_ = Parser.liftA2(New::new, ident, _equals._and(string("new")._and(_ws)._and(_expr)));
    Parser<Stmt> writeAt = Parser.liftA2(WriteAt::new, _expr, string(":=")._and(_ws)._and(_expr));

    Parser<Stmt> fork = string("fork")._and(_ws)._and(block).map(Fork::new);

    var stmt_ = choice(fork, writeAt, new_, repeatUntil, while_, if_, open, read, close, declAssign, decl, assign, print).and_(_ws).option(Nop.nop);
    var seq = stmt_.chainr1(ch(';').and_(_ws).map_(Seq::new));
    stmtFwdRef.snd.set(seq);

    _parser = shebang.option(Unit.UNIT)._and(_ws)._and(stmt).and_(eof);

    // Val parsers for reading files

    var valFwdRef = Parser.<Val>fwdRef();
    _val = valFwdRef.fst;

    Parser<Val> vInt = number.map(BigInteger::new).map(VInt::new);
    Parser<Val> vBool = boolRaw.map(VBool::new);
    Parser<Val> vStr = strRaw.map(VStr::new);

    Parser<Val> vRec = _record('{', _recField, _equals, _val).map(m -> new VRec(true, m));
    Parser<Val> vTup = _tuple(a -> new VRec(false, _tupToRec(a)), _val);

    var val = choice(vRec, vTup, vStr, vBool, vInt);
    valFwdRef.snd.set(val);

    _valLine = _ws._and(_val).and_(eof);
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

  private static <A> Map<Field, A> _tupToRec(List<A> as) {
    return Map.fromList(as.indexedWith(TupField::new));
  }

  private static Parser<Expr> _member(Parser<Expr> lhs) {
    var idxs = ch('.')._and(_tupField.or(_recField)).and_(_ws).many();
    return liftA2((a, b) -> b.foldL(RecMember::new, a), lhs, idxs);
  }

  private static Expr _mkLam(List<Pair<Ident, Type>> l, Expr e) {
    return l.match(
      () -> e,
      (a, as) -> new Lam(a.fst, a.snd, _mkLam(as, e)));
  }

  private static <I extends Comparable<I>> Parser<Expr> _withExpr(BiFunction<Expr, Map<I, Expr>, Expr> ctor, Parser<I> idx) {
    return liftA2(ctor, ch('{')._and(_ws)._and(_exprNoWith).and_(_ws), _record('|', idx, _equals, _expr));
  }

  private static char _unescape(char c) {
    return switch (c) {
      case '\\' -> '\\';
      case '"' -> '"';
      case '0' -> '\0';
      case 'n' -> '\n';
      case 'r' -> '\r';
      // Invalid in Java apparently
      // case 'v' -> '\v';
      case 't' -> '\t';
      case 'b' -> '\b';
      case 'f' -> '\f';
      default -> c;
    };
  }

  public static Stmt parse(String code) {
    return _parser.parse(code);
  }

  public static Val parseValLine(String file, String line) {
    return _valLine.run(line).match(
      () -> { throw new IncorrectFileFormatException(file); },
      (v, s) -> v);
  }
}
