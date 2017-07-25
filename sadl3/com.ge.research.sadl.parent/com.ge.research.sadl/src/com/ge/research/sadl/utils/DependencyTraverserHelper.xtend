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
package com.ge.research.sadl.utils

import com.google.common.base.Equivalence
import com.google.common.base.Objects
import com.google.common.base.Optional
import com.google.common.base.Preconditions
import com.google.common.collect.ImmutableList
import java.util.Stack
import com.google.common.collect.HashMultimap

/**
 * Helper for traversing dependencies.
 * 
 * @author akos.kitta
 */
class DependencyTraverserHelper {

	/**
	 * Returns with an identity equivalence. Checks equivalence via the identity hash code of the objects.
	 */
	static def <T> identity() {
		return new Equivalence<T>() {

			override protected doEquivalent(Object a, Object b) {
				return false;
			}

			override protected doHash(Object t) {
				return System.identityHashCode(t);
			}

		}
	}

	/**
	 * Returns with an equals equivalence. Checks equivalence via {@code hashCode} and {@code equals} of the objects. 
	 */
	static def <T> equals() {
		return new Equivalence<T>() {

			override protected doEquivalent(Object a, Object b) {
				return Objects.equal(a, b);
			}

			override protected doHash(Object t) {
				return Objects.hashCode(t);
			}

		}
	}

	/**
	 * Checks whether the dependency graph has any cycles or not. Returns with a cycle instance if the graph
	 * has cycle, otherwise the cycle is absent.
	 */
	def <T> Optional<Cycle<T>> checkCycle(T root, (T)=>Iterable<T> dependencies, extension Equivalence<T> equivalence) {
		Preconditions.checkNotNull(root, 'root');
		Preconditions.checkNotNull(dependencies, 'dependencies');
		Preconditions.checkNotNull(equivalence, 'equivalence');

		val stack = new Stack();
		val visited = HashMultimap.create();
		val chain = newArrayList();

		stack.push(root);
		while (!stack.empty()) {
			var node = stack.pop();
			chain.add(node);
			val wrappedNode = wrap(node);
			for (upstreamNode : dependencies.apply(node).filterDuplicates(equivalence)) {
				stack.push(upstreamNode);
				if (!visited.put(wrappedNode, wrap(upstreamNode))) {
					return Optional.of(createCycle(chain, equivalence));
				}
			}
		}

		return Optional.absent;
	}
	
	/**
	 * Creates and returns with the cycle with the chain of graph nodes (in the visiting order)
	 * and the equivalence.
	 */
	protected def <T> createCycle(Iterable<? extends T> chain, Equivalence<T> equivalence) {
		return new Cycle(chain, equivalence);
	}
	
	private def <T> filterDuplicates(Iterable<? extends T> nullable, extension Equivalence<T> equivalence) {
		return if (nullable === null) emptyList else nullable.map[wrap].toSet.map[get];
	}

	/**
	 * A dependency cycle.
	 */
	static class Cycle<T> {

		protected val Iterable<? extends T> chain;
		protected val Equivalence<T> equivalence;

		new(Iterable<? extends T> chain, Equivalence<T> equivalence) {
			Preconditions.checkArgument(!chain.nullOrEmpty, 'Expected non-empty chain of items.');
			this.chain = ImmutableList.copyOf(chain);
			this.equivalence = equivalence;
		}

		/**
		 * Prints the dependency chain that has a cycle.
		 */
		def prettyPrint() {
			return prettyPrint[String.valueOf(it)];
		}

		/**
		 * Sugar for printing the chain with the cycle in it.
		 */
		def prettyPrint((T)=>String toString) {
			val last = chain.last;
			chain.map [
				val label = toString.apply(it);
				val shouldEscape = equivalence.equivalent(last, it);
				return if (shouldEscape) '''[«label»]''' else label;
			].join(' -> ');
		}

	}

}
