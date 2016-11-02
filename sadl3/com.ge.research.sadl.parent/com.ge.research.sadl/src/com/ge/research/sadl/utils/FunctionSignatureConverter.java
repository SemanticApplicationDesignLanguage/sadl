package com.ge.research.sadl.utils;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.common.util.EList;

import com.ge.research.sadl.model.DeclarationExtensions;
import com.ge.research.sadl.model.gp.FunctionSignature;
import com.ge.research.sadl.sADL.BooleanLiteral;
import com.ge.research.sadl.sADL.Constant;
import com.ge.research.sadl.sADL.Expression;
import com.ge.research.sadl.sADL.Name;
import com.ge.research.sadl.sADL.NumberLiteral;
import com.ge.research.sadl.sADL.PropOfSubject;
import com.ge.research.sadl.sADL.SadlResource;
import com.ge.research.sadl.sADL.StringLiteral;
import com.ge.research.sadl.sADL.SubjHasProp;
import com.ge.research.sadl.sADL.UnaryExpression;

public class FunctionSignatureConverter {
	
	public static FunctionSignature convertNameToFunctionSignature(Name function, DeclarationExtensions declarationExtensions){
		String functionName = declarationExtensions.getConcreteName(function);
		String functionUri = declarationExtensions.getConceptUri(function);
		EList<Expression> parametersExpression = function.getArglist();
		List<String> parametersString = new ArrayList<String>();
		String conversionString = null;
		for(Expression e : parametersExpression){
			conversionString = convert(e);
			if(conversionString == null){
				continue;
			}
			parametersString.add(conversionString);
		}
		
		return new FunctionSignature(functionName, functionUri, parametersString);
	}

	private static String convert(Expression e){
		//BinaryOperation
		//Declaration
		//ElementInList
		//RequirementShallStatement
		//PreviousAwareName
		//TimeAwareOperation
		//ValueRow
		//ValueTable
		//AssertionExpression
		//AssumptionExpression
		//HasChanged
		//Sublist
		//Unit
		if (e instanceof BooleanLiteral) {
			return convert((BooleanLiteral)e);
		}
		else if (e instanceof Constant) {
			return convert((Constant)e);
		}
		else if (e instanceof Name) {
			return convert((Name)e);
		}
		else if (e instanceof NumberLiteral) {
			return convert((NumberLiteral)e);
		}
		else if (e instanceof PropOfSubject) {
			return convert((PropOfSubject)e);
		}
		else if (e instanceof SadlResource) {
			return convert((SadlResource)e);
		}
		else if (e instanceof StringLiteral) {
			return convert((StringLiteral)e);
		}
		else if (e instanceof SubjHasProp) {
			return convert((SubjHasProp)e);
		}
		else if (e instanceof UnaryExpression) {
			return convert((UnaryExpression)e);
		}
		
		return null;
	}

	private static String convert(UnaryExpression e) {
		// TODO Auto-generated method stub
		return null;
	}

	private static String convert(SubjHasProp e) {
		// TODO Auto-generated method stub
		return null;
	}

	private static String convert(StringLiteral e) {
		// TODO Auto-generated method stub
		return null;
	}

	private static String convert(SadlResource e) {
		// TODO Auto-generated method stub
		return null;
	}

	private static String convert(PropOfSubject e) {
		// TODO Auto-generated method stub
		return null;
	}

	private static String convert(NumberLiteral e) {
		// TODO Auto-generated method stub
		return null;
	}

	private static String convert(Name e) {
		// TODO Auto-generated method stub
		return null;
	}

	private static String convert(Constant e) {
		// TODO Auto-generated method stub
		return null;
	}

	private static String convert(BooleanLiteral e) {
		// TODO Auto-generated method stub
		return null;
	}
}
