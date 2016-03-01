package com.ge.research.sadl.processing;

import com.ge.research.sadl.model.DeclarationExtensions;
import com.ge.research.sadl.sADL.BinaryOperation;
import com.ge.research.sadl.sADL.BooleanLiteral;
import com.ge.research.sadl.sADL.Constant;
import com.ge.research.sadl.sADL.Declaration;
import com.ge.research.sadl.sADL.Expression;
import com.ge.research.sadl.sADL.Name;
import com.ge.research.sadl.sADL.NumberLiteral;
import com.ge.research.sadl.sADL.PropOfSubject;
import com.ge.research.sadl.sADL.StringLiteral;
import com.ge.research.sadl.sADL.SubjHasProp;
import com.google.inject.Inject;

public abstract class SadlModelProcessor implements ISadlModelProcessor {

	@Inject
	public DeclarationExtensions declarationExtensions;

	protected String processExpression(BinaryOperation expr) {
		StringBuilder sb = new StringBuilder();
		String op = expr.getOp();
		sb.append(processExpression(expr.getLeft()));
		sb.append(" ");
		sb.append(op);
		sb.append(" ");
		sb.append(processExpression(expr.getRight()));
		return sb.toString();
	}

	public abstract Object processExpression(Expression left) ;
	
	protected String processExpression(SubjHasProp expr) {
		StringBuilder sb = new StringBuilder();
		sb.append(processExpression(expr.getLeft()));
		sb.append(" ");
		sb.append(processExpression(expr.getProp()));
		sb.append(" ");
		sb.append(processExpression(expr.getRight()));
		return sb.toString();
	}

	protected String processExpression(PropOfSubject expr) {
		StringBuilder sb = new StringBuilder();
		sb.append(processExpression(expr.getLeft()));
		sb.append(" of ");
		sb.append(processExpression(expr.getRight()));
		return sb.toString();
	}

	protected String processExpression(Name expr) {
		return declarationExtensions.getConceptUri(expr.getName());
	
	}

	protected String processExpression(Declaration expr) {
		StringBuilder sb = new StringBuilder();
		String article = expr.getArticle();
		sb.append(article);
		sb.append(" ");
		sb.append(processExpression(expr.getType()));
		if (expr.getNewName() != null) {
			sb.append(" ");
			sb.append(expr.getNewName());
		}
		return sb.toString();
	}

	protected String processExpression(BooleanLiteral expr) {
		return expr.getValue();
	}

	protected String processExpression(NumberLiteral expr) {
		return expr.getValue().toPlainString();
	}

	protected String processExpression(StringLiteral expr) {
		return expr.getValue();
	}

	protected String processExpression(Constant expr) {
		return expr.getConstant();
	}

}
