package com.ge.research.sadl.reasoner;

import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;

public class TestConfigurationManager {

	@Before
	public void setUp() throws Exception {
	}

	@Test
	public void testConfigurationManager() {
		IConfigurationManager icm = new ConfigurationManager();
		assertNotNull(icm);
	}

}
