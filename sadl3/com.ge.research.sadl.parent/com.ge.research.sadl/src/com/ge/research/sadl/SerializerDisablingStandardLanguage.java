package com.ge.research.sadl;

import java.util.List;

import org.eclipse.xtext.xtext.generator.IXtextGeneratorFragment;
import org.eclipse.xtext.xtext.generator.StandardLanguage;

public class SerializerDisablingStandardLanguage extends StandardLanguage {

	@Override
	protected List<? extends IXtextGeneratorFragment> getImplicitFragments() {
		List<? extends IXtextGeneratorFragment> implicitFragments = super.getImplicitFragments();
		implicitFragments.remove(getSerializer());
		return implicitFragments;
	}
}
