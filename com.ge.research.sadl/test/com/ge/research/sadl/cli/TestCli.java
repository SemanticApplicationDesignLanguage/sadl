package com.ge.research.sadl.cli;

import static org.junit.Assert.*;

import java.io.IOException;
import java.util.Iterator;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.validation.Issue;
import org.junit.Before;
import org.junit.Test;

import com.google.common.collect.Multimap;

public class TestCli {

	@Before
	public void setUp() throws Exception {
	}

	@Test
	public void testtest() {
		System.out.println("Hi");
	}

	@Test
	public void testCBMRules () throws IOException, Exception {
//		String projectPath = "c:/tmp/CBMSadlRules";
		String projectPath = "c:/tmp/SadlProject1";
//		String projectPath = "c:/tmp/PlugInNoOwlModels";
		SadlCli cli = new SadlCli();
		assertNotNull(cli);
		String outputFolder = cli.processProject(projectPath);
		assertNotNull(outputFolder);
		Multimap<Resource, Issue> errors = cli.getErrors();
		if (errors == null || errors.size() == 0) {
			System.out.println("Successfully built project, output at '" + outputFolder + "'");
		}
		else {
			System.err.println("Project build encountered errors:");
			Iterator<Resource> itr = errors.keySet().iterator();
			while (itr.hasNext()) {
				Resource r = itr.next();
				System.err.println("   Resource '" + r.toString() + "' errors:");
				Iterator<Issue> issueItr = errors.get(r).iterator();
				while (issueItr.hasNext()) {
					Issue issue = issueItr.next();
					System.err.println(issue.toString());
				}
			}
		}
	}

}
