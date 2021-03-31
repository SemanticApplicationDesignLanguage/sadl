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

External addOne(decimal X) returns decimal:
"org.apache.jena.reasoner.rulesys.builtins#addOne".

External bound(string X) returns boolean:
"org.apache.jena.reasoner.rulesys.builtins#bound".

External countLiteralValues(string X, string X) returns int:
"org.apache.jena.reasoner.rulesys.builtins#countLiteralValues".

External difference(decimal X, decimal X) returns decimal:
"org.apache.jena.reasoner.rulesys.builtins#difference".

External ge(decimal X, decimal X) returns boolean:
"org.apache.jena.reasoner.rulesys.builtins#ge".

External le(decimal X, decimal X) returns boolean:
"org.apache.jena.reasoner.rulesys.builtins#le".

External isBNode(string X) returns boolean:
"org.apache.jena.reasoner.rulesys.builtins#isBNode".

External isDType(string X) returns boolean:
"org.apache.jena.reasoner.rulesys.builtins#isDType".

External isLiteral(string X) returns boolean:
"org.apache.jena.reasoner.rulesys.builtins#isLiteral".

External listContains(string X, string X) returns boolean:
"org.apache.jena.reasoner.rulesys.builtins#listContains".

External listEntry(string X, int X) returns string:
"org.apache.jena.reasoner.rulesys.builtins#listEntry".

External listEqual(string X, string X) returns boolean:
"org.apache.jena.reasoner.rulesys.builtins#listEqual".

External listLength(string X) returns int:
"org.apache.jena.reasoner.rulesys.builtins#listLength".

External listMapAsObject(string X, string X, string X) returns boolean:
"org.apache.jena.reasoner.rulesys.builtins#listMapAsObject".

External listMapAsSubject(string X, string X, string X) returns boolean:
"org.apache.jena.reasoner.rulesys.builtins#listMapAsSubject".

External listNotContains(string X, string X) returns boolean:
"org.apache.jena.reasoner.rulesys.builtins#listNotContains".

External listNotEqual(string X, string X) returns boolean:
"org.apache.jena.reasoner.rulesys.builtins#listNotEqual".

External notBNode(string X) returns boolean:
"org.apache.jena.reasoner.rulesys.builtins#notBNode".

External notBType(string X) returns boolean:
"org.apache.jena.reasoner.rulesys.builtins#notBType".

External notDType(string X) returns boolean:
"org.apache.jena.reasoner.rulesys.builtins#notDType".

External notLiteral(string X) returns boolean:
"org.apache.jena.reasoner.rulesys.builtins#notLiteral".

External now() returns dateTime:
"org.apache.jena.reasoner.rulesys.builtins#now".

External regex(string X, string X) returns string:
"org.apache.jena.reasoner.rulesys.builtins#regex".

External strConcat(string X, ...) returns string:
"org.apache.jena.reasoner.rulesys.builtins#strConcat".

External uriConcat(string X, string X) returns string:
"org.apache.jena.reasoner.rulesys.builtins#uriConcat".

External pow(decimal X, decimal X) returns decimal:
"org.apache.jena.reasoner.rulesys.builtins#pow".

External sqrt(decimal X) returns decimal:
"org.apache.jena.reasoner.rulesys.builtins#sqrt".

External unbound(string X) returns string:
"org.apache.jena.reasoner.rulesys.builtins#unbound".

External firstElement(--) returns --:
"com.naturalsemantics.sadl.jena.reasoner.builtin#firstElement".

External insertElementInList(--, --, int X) returns --:
"com.naturalsemantics.sadl.jena.reasoner.builtin#insertElementInList".

External elementInList(--, int X) returns --:
"com.naturalsemantics.sadl.jena.reasoner.builtin#elementInList".

External lastElement(--) returns --:
"com.naturalsemantics.sadl.jena.reasoner.builtin#lastElement".

External deleteElementFromList(--, int X) returns --:
"com.naturalsemantics.sadl.jena.reasoner.builtin#deleteElementFromList".

External elementAfter(--, --) returns --:
"com.naturalsemantics.sadl.jena.reasoner.builtin#elementAfter".

External sadlListToString(--) returns string:
"com.naturalsemantics.sadl.jena.reasoner.builtin#sadlListToString".

External ^length(--) returns int:
"com.naturalsemantics.sadl.jena.reasoner.builtin#length".

External ^index(--, --) returns int:
"com.naturalsemantics.sadl.jena.reasoner.builtin#index".

External elementBefore(--, --) returns --:
"com.naturalsemantics.sadl.jena.reasoner.builtin#elementBefore".

External isListHead(--):
"com.naturalsemantics.sadl.jena.reasoner.builtin#isListHead".

External localname(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin#localname".

External acos(decimal X) returns decimal:
"com.ge.research.sadl.jena.reasoner.builtin#acos".

External noUnknownValues(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin#noUnknownValues".

External min(decimal X, decimal X, ...) returns decimal:
"com.ge.research.sadl.jena.reasoner.builtin#min".

External assign(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin#assign".

External tan(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin#tan".

External sum(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin#sum".

External greaterThan(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin#greaterThan".

External list(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin#list".

External listConcat(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin#listConcat".

External listToString(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin#listToString".

External thereExists(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin#thereExists".

External ceiling(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin#ceiling".

External abs(decimal X) returns decimal:
"com.ge.research.sadl.jena.reasoner.builtin#abs".

External countUniqueMatches(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin#countUniqueMatches".

External noSubjectsOtherThan(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin#noSubjectsOtherThan".

External print(--, ...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin#print".

External atan(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin#atan".

External countMatches(...) returns int:
"com.ge.research.sadl.jena.reasoner.builtin#countMatches".

External notOnlyValue(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin#notOnlyValue".

External noValue(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin#noValue".

External oneOf(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin#oneOf".

External cos(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin#cos".

External sin(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin#sin".

External subtractDates(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin#subtractDates".

External pow(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin#pow".

External max(decimal X, decimal X, ...) returns decimal:
"com.ge.research.sadl.jena.reasoner.builtin#max".

External ^unique(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin#unique".

External product(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin#product".

External asin(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin#asin".

External lessThan(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin#lessThan".

External average(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin#average".

External floor(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin#floor".

External getClassFromConstraint(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin#getClassFromConstraint".

External mod(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin#mod".

External listSubtract(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin#listSubtract".

External noValuesOtherThan(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin#noValuesOtherThan".

External sqrt(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin#sqrt".

External getInstance(...) returns --:
"com.ge.research.sadl.jena.reasoner.builtin#getInstance".

External ^table(--) returns --:
"org.apache.jena.reasoner.rulesys.builtins#table".

External tableAll(--) returns --:
"org.apache.jena.reasoner.rulesys.builtins#tableAll".
	'''

}
