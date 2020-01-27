package com.ge.research.sadl.validation

import com.google.common.base.Equivalence
import java.util.Arrays
import java.util.Set
import org.eclipse.xtext.validation.Issue

final class IssueUtils {

	/**
	 * Returns with a distinct set of the issues argument.
	 */
	static def Set<Issue> distinct(Iterable<? extends Issue> issues) {
		return issues.map[IssueEquivalence.INSTANCE.wrap(it)].toSet.map[get].toSet
	}

	private new() {
	}

	private static class IssueEquivalence extends Equivalence<Issue> {
		
		static val INSTANCE = new IssueEquivalence();

		override protected boolean doEquivalent(Issue left, Issue right) {
			return left.uriToProblem.toString == right.uriToProblem.toString
				&& left.message == right.message
				&& left.code == right.code
				&& left.severity == right.severity
				&& left.offset === right.offset
				&& left.lineNumber === right.lineNumber
				&& left.column === right.column
				&& left.length === right.length
				&& Arrays.equals(left.data, right.data)
		}

		override protected int doHash(Issue it) {
			return 0; // Use #doEquivalent
		}

	}
}
