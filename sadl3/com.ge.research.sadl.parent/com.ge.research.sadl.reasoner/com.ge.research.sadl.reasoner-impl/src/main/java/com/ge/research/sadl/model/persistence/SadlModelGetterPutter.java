package com.ge.research.sadl.model.persistence;

import com.ge.research.sadl.model.persistence.ISadlModelGetterPutter;
import com.ge.research.sadl.reasoner.IConfigurationManager;

abstract public class SadlModelGetterPutter extends SadlModelGetter implements ISadlModelGetterPutter {

	public SadlModelGetterPutter(IConfigurationManager mgr, String fmt) {
		super(mgr, fmt);
	}

}
