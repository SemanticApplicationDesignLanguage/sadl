/************************************************************************
 * Copyright 2007-2016- General Electric Company, All Rights Reserved
 * 
 * Project: SADL
 * 
 * Description: The Semantic Application Design Language (SADL) is a
 * language for building semantic models and expressing rules that
 * capture additional domain knowledge. The SADL-IDE (integrated
 * development environment) is a set of Eclipse plug-ins that
 * support the editing and testing of semantic models using the
 * SADL language.
 * 
 * This software is distributed "AS-IS" without ANY WARRANTIES
 * and licensed under the Eclipse Public License - v 1.0
 * which is available at http://www.eclipse.org/org/documents/epl-v10.php
 * 
 ***********************************************************************/
package com.ge.research.sadl.tests

/**
 * Helper for tests.
 * 
 * @author akos.kitta
 */
class SadlTestHelper {

	/**
	 * The built-in functions content.
	 * 
	 * <p>
	 * Normally, this is retrieved via the configuration manager for IDE > translator > content.
	 * For tests this is just a string unless one figures out how to deal with the configuration manager
	 * in a headless tests.
	 * 
	 * NOTE: this must be updated if any functions are used in tests with the default reasoner/translator that are not in this content.
	 */
	public static val SADL_BUILTIN_FUNCTIONS_CONTENT = '''
uri "http://sadl.org/builtinfunctions" alias builtinfunctions.

External noUnknownValues(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.NoUnknownValues".

External sqrt(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.Sqrt".

External cos(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.Cos".

External strafter(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.StrAfter".

External list(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.List".

External asin(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.Asin".

External sum(decimal ... x1) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.Sum".

External greaterThan(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.GreaterThan".

External mod(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.Mod".

External min(decimal X1, decimal ... X2) returns decimal:
"com.ge.research.sadl.jena.reasoner.builtin.Min".

External floor(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.Floor".

External product(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.Product".

External ceiling(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.Ceiling".

External max(decimal X, decimal X, ...) returns decimal:
"com.ge.research.sadl.jena.reasoner.builtin.Max".

External print(...):
"com.ge.research.sadl.jena.reasoner.builtin.Print".

External countMatches(...) returns int:
"com.ge.research.sadl.jena.reasoner.builtin.CountMatches".

External tan(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.Tan".

External countUniqueMatches(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.CountUniqueMatches".

External lessThan(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.LessThan".

External listConcat(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.ListConcat".

External listToString(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.ListToString".

External localname(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.Localname".

External getInstance(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.GetInstance".

External noValuesOtherThan(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.NoValuesOtherThan".

External sin(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.Sin".

External pow(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.Pow".

External noValue(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.NoValue".

External notOnlyValue(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.NotOnlyValue".

External assign(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.Assign".

External atan(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.Atan".

External average(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.Average".

External sameAs(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.SameAs".

External acos(decimal X) returns decimal:
"com.ge.research.sadl.jena.reasoner.builtin.Acos".

External abs(decimal X) returns decimal:
"com.ge.research.sadl.jena.reasoner.builtin.Abs".

External oneOf(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.OneOf".

External listSubtract(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.ListSubtract".

External differentFrom(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.DifferentFrom".

External ^unique(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.Unique".

External thereExists(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.ThereExists".

External subtractDates(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.SubtractDates".

External combineUnits(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.CombineUnits".

External getClassFromConstraint(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.GetClassFromConstraint".

External strbefore(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.StrBefore".

External noSubjectsOtherThan(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin.NoSubjectsOtherThan".

External ^index(--, --) returns int:
"com.naturalsemantics.sadl.jena.reasoner.builtin.Index".

External insertElementInList(--, --, int X) returns --:
"com.naturalsemantics.sadl.jena.reasoner.builtin.InsertElementInList".

External elementInList(--, int X) returns --:
"com.naturalsemantics.sadl.jena.reasoner.builtin.ElementInList".

External lastElement(--) returns --:
"com.naturalsemantics.sadl.jena.reasoner.builtin.LastElement".

External evaluateSadlEquation(...) returns --:
"com.naturalsemantics.sadl.jena.reasoner.builtin.EvaluateSadlEquation".

External isListHead(--):
"com.naturalsemantics.sadl.jena.reasoner.builtin.IsListHead".

External elementBefore(--, --) returns --:
"com.naturalsemantics.sadl.jena.reasoner.builtin.ElementBefore".

External deleteElementFromList(--, int X) returns --:
"com.naturalsemantics.sadl.jena.reasoner.builtin.DeleteElementFromList".

External elementAfter(--, --) returns --:
"com.naturalsemantics.sadl.jena.reasoner.builtin.ElementAfter".

External sadlListToString(--) returns string:
"com.naturalsemantics.sadl.jena.reasoner.builtin.SadlListToString".

External listLength(--) returns int:
"com.naturalsemantics.sadl.jena.reasoner.builtin.ListLength".

External firstElement(--) returns --:
"com.naturalsemantics.sadl.jena.reasoner.builtin.FirstElement".

External addOne(--) returns --:
"org.apache.jena.reasoner.rulesys.builtins.AddOne".

External bound(--) returns --:
"org.apache.jena.reasoner.rulesys.builtins.Bound".

External countLiteralValues(--) returns --:
"org.apache.jena.reasoner.rulesys.builtins.CountLiteralValues".

External difference(--) returns --:
"org.apache.jena.reasoner.rulesys.builtins.Difference".

External ge(--) returns --:
"org.apache.jena.reasoner.rulesys.builtins.GE".

External isBNode(--) returns --:
"org.apache.jena.reasoner.rulesys.builtins.IsBNode".

External isDType(--) returns --:
"org.apache.jena.reasoner.rulesys.builtins.IsDType".

External isLiteral(--) returns --:
"org.apache.jena.reasoner.rulesys.builtins.IsLiteral".

External le(--) returns --:
"org.apache.jena.reasoner.rulesys.builtins.LE".

External listContains(--) returns --:
"org.apache.jena.reasoner.rulesys.builtins.ListContains".

External listEntry(--) returns --:
"org.apache.jena.reasoner.rulesys.builtins.ListEntry".

External listEqual(--) returns --:
"org.apache.jena.reasoner.rulesys.builtins.ListEqual".

// listLength (org.apache.jena.reasoner.rulesys.builtins.ListLength) is overridden by com.naturalsemantics.sadl.jena.reasoner.builtin.ListLength

External listMapAsObject(--) returns --:
"org.apache.jena.reasoner.rulesys.builtins.ListMapAsObject".

External listMapAsSubject(--) returns --:
"org.apache.jena.reasoner.rulesys.builtins.ListMapAsSubject".

External listNotContains(--) returns --:
"org.apache.jena.reasoner.rulesys.builtins.ListNotContains".

External listNotEqual(--) returns --:
"org.apache.jena.reasoner.rulesys.builtins.ListNotEqual".

// min (org.apache.jena.reasoner.rulesys.builtins.Min) is overridden by com.ge.research.sadl.jena.reasoner.builtin.Min

External notBNode(--) returns --:
"org.apache.jena.reasoner.rulesys.builtins.NotBNode".

External notDType(--) returns --:
"org.apache.jena.reasoner.rulesys.builtins.NotDType".

External notLiteral(--) returns --:
"org.apache.jena.reasoner.rulesys.builtins.NotLiteral".

External now(--) returns --:
"org.apache.jena.reasoner.rulesys.builtins.Now".

External regex(--) returns --:
"org.apache.jena.reasoner.rulesys.builtins.Regex".

External strConcat(--) returns --:
"org.apache.jena.reasoner.rulesys.builtins.StrConcat".

External ^table(--) returns --:
"org.apache.jena.reasoner.rulesys.builtins.Table".

External tableAll(--) returns --:
"org.apache.jena.reasoner.rulesys.builtins.TableAll".

External unbound(--) returns --:
"org.apache.jena.reasoner.rulesys.builtins.Unbound".

External uriConcat(--) returns --:
"org.apache.jena.reasoner.rulesys.builtins.UriConcat".
	'''

}
