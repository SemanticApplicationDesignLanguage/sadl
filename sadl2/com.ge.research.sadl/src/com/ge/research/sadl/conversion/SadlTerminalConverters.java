package com.ge.research.sadl.conversion;

import org.eclipse.xtext.common.services.DefaultTerminalConverters;
import org.eclipse.xtext.conversion.IValueConverter;
import org.eclipse.xtext.conversion.ValueConverter;

import com.google.inject.Inject;

public class SadlTerminalConverters extends DefaultTerminalConverters {
	@Inject
	private NAMEValueConverter nameValueConverter;

	@ValueConverter(rule = "NAME")
	public IValueConverter<String> NAME() {
		return nameValueConverter;
	}

}
