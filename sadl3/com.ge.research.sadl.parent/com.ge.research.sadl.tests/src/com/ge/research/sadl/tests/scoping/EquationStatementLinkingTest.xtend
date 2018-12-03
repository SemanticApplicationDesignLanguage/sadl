package com.ge.research.sadl.tests.scoping

import com.ge.research.sadl.tests.AbstractLinkingTest
import org.junit.Test

/**
 * From: https://github.com/crapo/sadlos2/issues/344#issue-382432616
 * <ul>
 * <li><b>a)</b> If a variable is defined in the "where" clause and used in the body, the definition is the reference in the "where" clause.</li>
 * <li><b>b)</b> If a variable is referenced in the body and referenced in the "return" statement, the definition is the reference in the body.</li>
 * </ul>
 */
class EquationStatementLinkingTest extends AbstractLinkingTest {

	@Test
	def void testEquationStatementScoping_GH_344_1() {
		'''
			// ------ SETUP ------
			uri "http://sadl.org/MinimalExample.sadl" alias mex.
			ScientificConcept is a class.
			sadlimplicitmodel:UnittedQuantity is a type of ScientificConcept.
			External unitResolver(string u, ...) returns string: "http://sadl.org/unitSelector".
			External derivative(ScientificConcept numerator, ScientificConcept denominator, int n) returns decimal, string: "http://sadl.org/derivative".
			Mass is a type of UnittedQuantity.
			Velocity is a type of UnittedQuantity.
			Momentum is a type of UnittedQuantity.
			Force is a type of UnittedQuantity.
			Time is a type of UnittedQuantity.
			// ------ END OF SETUP ------
			
			Equation newtons2ndLaw
			  (note "Force is equal to the derivative of momentum with respect to time.")
			  (Mass [m], Velocity [v]) returns Force:
			// BODY
			  a Force [f] with ^value <fv>, with unit <fu>
			
			// RETURN
			  return <f>
			
			// WHERE
			  where [mv] is a Momentum
			    with ^value (^value of <m> * ^value of <v>),
			    with unit unitResolver("*", unit of <m>, unit of <v>)
			    and [[fv],[fu]] = derivative(<mv>, Time, 1).
		'''.assertLinking[sadl];
	}

	@Test
	def void testEquationStatementScoping_GH_344_2() {
		'''
			// ------ SETUP ------
			uri "http://sadl.org/ScientificConcepts1.sadl" alias scicncpts1.
			Time is a type of UnittedQuantity.
			Length is a type of UnittedQuantity.
			Position is a type of UnittedQuantity,
				described by x-coordinate with values of type Length,
				described by y-coordinate with values of type Length,
				described by z-coordinate with values of type Length,
				described by ^time with values of type Time.
			Mass is a type of UnittedQuantity.
			PhysicalObject is a class,
				described by mass with values of type Mass,
				described by position with values of type Position.
			Velocity is a type of UnittedQuantity.
			velocity describes PhysicalObject with values of type Velocity.
			Acceleration is a type of UnittedQuantity.
			acceleration describes PhysicalObject with values of type Acceleration.
			Momentum is a type of UnittedQuantity.
			momentum describes PhysicalObject with values of type Momentum.
			Force is a type of UnittedQuantity.
			force describes PhysicalObject with values of type Force.
			External unitResolver(string operation, string u, ...) 
				returns string: "http://sadl.org/unitSelector".
			External derivative(ScientificConcept numerator, anyURI ^time, int n) 
				returns decimal, string: "http://sadl.org/derivative".
			ScientificConcept is a class.
			sadlimplicitmodel:UnittedQuantity is a type of ScientificConcept.
			// ------ END OF SETUP ------
			
			Equation newtons2ndLaw
				(note "net Force on a physical object is equal to the derivative of the momentum of the object with respect to time.")
				(PhysicalObject [o]) returns Force: a Force [f] with ^value <fv>, with unit <fu>
					return <f>
						where [mv] is a Momentum with ^value (^value of mass of <o> * ^value of velocity of <o>), 
							with unit unitResolver("*", unit of mass of <o>, unit of velocity of <o>) and
							[[fv],[fu]] = derivative(<mv>, ^time, 1). 
				
			Equation newtons2ndLawConstantMass
				(note "net Force on a physical object is equal to the mass of the object times its acceleration for constant mass.")
				(PhysicalObject [o]) returns Force: a Force [f] with ^value <fv>, with unit <fu>
					return <f>
						where [acc] is an Acceleration with ^value <accv>, with unit <accu> and
							[[mdotv], [mdotu]] is derivative(mass of <o>, ^time, 1) and 
							<mdotv> is 0 and
							[[accv],[accu]] = derivative(velocity of <o>, ^time, 1) and
							[fv] = ^value of mass of <o> * <accv> and
							[fu] = unitResolver("*", unit of mass of <o>, <accu>).
		'''.assertLinking[sadl];
	}

}
