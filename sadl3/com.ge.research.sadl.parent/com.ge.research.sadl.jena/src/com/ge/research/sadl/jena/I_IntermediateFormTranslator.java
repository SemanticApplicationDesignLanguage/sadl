package com.ge.research.sadl.jena;

import java.util.List;

import com.ge.research.sadl.model.gp.GraphPatternElement;
import com.ge.research.sadl.model.gp.Junction;
import com.ge.research.sadl.model.gp.Rule;

public interface I_IntermediateFormTranslator {

	// the target can be a Test, a Query, or a Rule instance
	void setTarget(Object _target);

	List<IFTranslationError> getErrors();

	Object cook(Object toCookObj);

	List<GraphPatternElement> listToAnd(List<GraphPatternElement> patterns);

	void setStartingVariableNumber(int vn);

	int getVariableNumber();

	void flattenJunction(Junction element);

	void setEncapsulatingTarget(Object _encapsulatingTarget);

}