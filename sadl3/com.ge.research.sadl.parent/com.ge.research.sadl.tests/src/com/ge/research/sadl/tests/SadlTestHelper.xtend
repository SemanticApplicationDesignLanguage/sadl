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
		
		External difference(decimal X,  decimal X) returns decimal:
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
		
		External abs(decimal X) returns decimal:
		"com.ge.research.sadl.jena.reasoner.builtin#abs".
		
		External acos(decimal X) returns decimal:
		"com.ge.research.sadl.jena.reasoner.builtin#acos".
		
		External asin(--) returns --:
		"com.ge.research.sadl.jena.reasoner.builtin#asin".
		
		External assign(--) returns --:
		"com.ge.research.sadl.jena.reasoner.builtin#assign".
		
		External atan(--) returns --:
		"com.ge.research.sadl.jena.reasoner.builtin#atan".
		
		External average(--) returns --:
		"com.ge.research.sadl.jena.reasoner.builtin#average".
		
		External ceiling(--) returns --:
		"com.ge.research.sadl.jena.reasoner.builtin#ceiling".
		
		External cos(--) returns --:
		"com.ge.research.sadl.jena.reasoner.builtin#cos".
		
		External countMatches(...) returns int:
		"com.ge.research.sadl.jena.reasoner.builtin#countMatches".
		
		External countUniqueMatches(--) returns --:
		"com.ge.research.sadl.jena.reasoner.builtin#countUniqueMatches".

		External floor(--) returns --:
		"com.ge.research.sadl.jena.reasoner.builtin#floor".
		
		External getClassFromConstraint(--) returns --:
		"com.ge.research.sadl.jena.reasoner.builtin#getClassFromConstraint".
		
		External getInstance(--) returns --:
		"com.ge.research.sadl.jena.reasoner.builtin#getInstance".

		External greaterThan(--) returns --:
		"com.ge.research.sadl.jena.reasoner.builtin#greaterThan".
		
		External lessThan(--) returns --:
		"com.ge.research.sadl.jena.reasoner.builtin#lessThan".
		
		External list(--) returns --:
		"com.ge.research.sadl.jena.reasoner.builtin#list".
		
		External listConcat(--) returns --:
		"com.ge.research.sadl.jena.reasoner.builtin#listConcat".
		
		External listSubtract(--) returns --:
		"com.ge.research.sadl.jena.reasoner.builtin#listSubtract".
		
		External listToString(--) returns --:
		"com.ge.research.sadl.jena.reasoner.builtin#listToString".

		External localname(--) returns --:
		"com.ge.research.sadl.jena.reasoner.builtin#localname".
		
		External max(decimal x, decimal y, ...) returns decimal:
		"com.ge.research.sadl.jena.reasoner.builtin#max".
		
		External min(decimal x,decimal y, ...) returns decimal:
		"com.ge.research.sadl.jena.reasoner.builtin#min".
		
		External mod(--) returns --:
		"com.ge.research.sadl.jena.reasoner.builtin#mod".
		
		External noSubjectsOtherThan(--) returns --:
		"com.ge.research.sadl.jena.reasoner.builtin#noSubjectsOtherThan".
		
		External notOnlyValue(--) returns --:
		"com.ge.research.sadl.jena.reasoner.builtin#notOnlyValue".
		
		External noUnknownValues(--) returns --:
		"com.ge.research.sadl.jena.reasoner.builtin#noUnknownValues".
		
		External noValue(--) returns --:
		"com.ge.research.sadl.jena.reasoner.builtin#noValue".

		External noValuesOtherThan(--) returns --:
		"com.ge.research.sadl.jena.reasoner.builtin#noValuesOtherThan".
		
		External oneOf(--) returns --:
		"com.ge.research.sadl.jena.reasoner.builtin#oneOf".

		External pow(--) returns --:
		"com.ge.research.sadl.jena.reasoner.builtin#pow".
		
		External print(string X, ...) returns --:
		"com.ge.research.sadl.jena.reasoner.builtin#print".
		
		External product(--) returns --:
		"com.ge.research.sadl.jena.reasoner.builtin#product".
		
		External sin(--) returns --:
		"com.ge.research.sadl.jena.reasoner.builtin#sin".
		
		External sqrt(--) returns --:
		"com.ge.research.sadl.jena.reasoner.builtin#sqrt".
		
		External subtractDates(--) returns --:
		"com.ge.research.sadl.jena.reasoner.builtin#subtractDates".
		
		External sum(--) returns --:
		"com.ge.research.sadl.jena.reasoner.builtin#sum".
		
		External tan(--) returns --:
		"com.ge.research.sadl.jena.reasoner.builtin#tan".
		
		External thereExists(--) returns --:
		"com.ge.research.sadl.jena.reasoner.builtin#thereExists".
		
		External ^unique(--) returns --:
		"com.ge.research.sadl.jena.reasoner.builtin#unique".

		External table(--) returns --:
		"org.apache.jena.reasoner.rulesys.builtins#table".
		
		External tableAll(--) returns --:
		"org.apache.jena.reasoner.rulesys.builtins#tableAll".
	'''

}
