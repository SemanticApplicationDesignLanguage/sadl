/************************************************************************
 * Copyright © 2007-2017 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.tests.imports

import com.ge.research.sadl.utils.DependencyTraverserHelper
import org.junit.Assert
import org.junit.Test

/**
 * Dependency traverser tests.
 * 
 * @author akos.kitta
 */
class DependencyTraverserHelperTest extends Assert {
	
	@Test
	def void checkNoCycle() {
		val graph = #{
			1 -> #[2],
			2 -> #[3]
		};
		val actual = new DependencyTraverserHelper().checkCycle(1, [graph.get(it)], DependencyTraverserHelper.equals);
		assertFalse(actual.present);
	}

	@Test
	def void checkRedundantImportIsNoCycle() {
		val graph = #{
			1 -> #[2, 2],
			2 -> #[3, 3],
			3 -> #[4, 4]
		};
		val actual = new DependencyTraverserHelper().checkCycle(1, [graph.get(it)], DependencyTraverserHelper.equals);
		assertFalse(actual.present);
	}
	
	@Test
	def void checkExplicitReImportIsNoCycle() {
		val graph = #{
			1 -> #[2, 3],
			2 -> #[3],
			3 -> #[]
		};
		val actual = new DependencyTraverserHelper().checkCycle(1, [graph.get(it)], DependencyTraverserHelper.equals);
		assertFalse(actual.present);
	}
	
	@Test
	def void checkSelfReference() {
		val graph = #{
			1 -> #[1]
		};
		val actual = new DependencyTraverserHelper().checkCycle(1, [graph.get(it)], DependencyTraverserHelper.equals);
		assertEquals(actual.get.prettyPrint, '[1] -> [1]');
	}
	
	@Test
	def void checkSimpleCycle() {
	val graph = #{
			1 -> #[2],
			2 -> #[1]
		};
		val actual = new DependencyTraverserHelper().checkCycle(1, [graph.get(it)], DependencyTraverserHelper.equals);
		assertEquals(actual.get.prettyPrint, '[1] -> 2 -> [1]');
	}
	
	@Test
	def void checkTransitiveCycle() {
		val graph = #{
			1 -> #[2],
			2 -> #[3],
			3 -> #[1]
		};
		val actual = new DependencyTraverserHelper().checkCycle(1, [graph.get(it)], DependencyTraverserHelper.equals);
		assertEquals(actual.get.prettyPrint, '[1] -> 2 -> 3 -> [1]');
	}

	@Test
	def void checkHook() {
		val graph = #{
			1 -> #[2],
			2 -> #[3],
			3 -> #[4],
			4 -> #[3]
		};
		val actual = new DependencyTraverserHelper().checkCycle(1, [graph.get(it)], DependencyTraverserHelper.equals);
		assertEquals(actual.get.prettyPrint, '1 -> 2 -> [3] -> 4 -> [3]');
	} 
	
}
