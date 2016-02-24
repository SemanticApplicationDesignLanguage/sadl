package com.ge.research.sadl.prolog.reasoner;

import static org.junit.Assert.*;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import alice.tuprolog.InvalidTheoryException;
import alice.tuprolog.MalformedGoalException;
import alice.tuprolog.NoMoreSolutionException;
import alice.tuprolog.NoSolutionException;
import alice.tuprolog.Prolog;
import alice.tuprolog.SolveInfo;
import alice.tuprolog.Term;
import alice.tuprolog.Theory;
import alice.tuprolog.Var;

public class TestPrologMobius {

	@Before
	public void setUp() throws Exception {
	}

	@Test
	public void test() throws InvalidTheoryException, FileNotFoundException, IOException, MalformedGoalException, NoSolutionException, NoMoreSolutionException {
		String kbfolder = "E:/sadl/workspace-sadl/Mobius2.new/OwlModels/";
		String rdffile = "rdf.pl";
		String stddecls = "tuprolog-custom-predicates.pl";
		String entry = "PlanRecloserInstance.pl";
		Prolog engine = new Prolog();
		engine.clearTheory();
		String fn = kbfolder + stddecls;
		System.out.println("Loading " + fn);
		engine.addTheory(createTheory(fn));
		fn = kbfolder + entry;
		System.out.println("Loading " + fn);
		engine.addTheory(createTheory(fn));
		fn = kbfolder + rdffile;
		System.out.println("Loading " + fn);
		engine.addTheory(createTheory(fn));
		SolveInfo result = engine.solve("minSkillProficiency('http://www.mobius.illinois.edu/advise/ont/core/Attack#AdminModifyFWOpen',Sk,P).");
//		SolveInfo result = engine.solve("preconditions(X,Y,Z).");
//		SolveInfo result = engine.solve("holds(X,Y,Z).");
		System.out.println(result.toString());
		assertNotNull(result);
		assertNotNull(result.getSolution());
		int solution_count = 0;
		List<String> solution_list = new ArrayList<String>();
		while (result.isSuccess()) {
			solution_count += 1;
			List<Var> vars = result.getBindingVars();
			StringBuilder sb = new StringBuilder();
			for (Var var: vars){
				sb.append(var.getName());
				sb.append(": ");
				sb.append(result.getVarValue(var.getName()).toString());
			}
			solution_list.add(sb.toString());
			if (engine.hasOpenAlternatives())
				result = engine.solveNext();
			else
				break;
		}
		for (int i = 0; i < solution_list.size(); i++) {
			System.out.println("Solution " + i + ": " + solution_list.get(i));
		}
	}

	@Test
	public void test2() throws InvalidTheoryException, FileNotFoundException, IOException, MalformedGoalException, NoSolutionException, NoMoreSolutionException {
		String kbfolder = "E:/sadl/workspace-sadl/Mobius2.new/OwlModels";
		Prolog engine = new Prolog();
		engine.clearTheory();
		File plFilesFolder = new File(kbfolder);
		File[] files = plFilesFolder.listFiles(); 
		// first load prolog files and then owl/rdf files
		for (int i = 0; i < files.length; i++){
			String fn = files[i].getName();
			if (fn.endsWith(".pl") && 
					(fn.startsWith("PlanR") || fn.startsWith("plann") || fn.startsWith("pr") || fn.startsWith("r") || fn.startsWith("b"))) {
				System.out.println("Loading " + files[i].getAbsolutePath());
				try {
					engine.addTheory(createTheory(files[i].getAbsolutePath()));
				} catch (InvalidTheoryException e) {
					// TODO Auto-generated catch block
					System.err.println("Syntax error: " + e.getMessage());
				}
			}
		}
		
		SolveInfo result = engine.solve("minSkillProficiency('http://www.mobius.illinois.edu/advise/ont/core/Attack#AdminModifyFWOpen',Sk,P).");
		System.out.println(result.toString());
		assertNotNull(result);
		assertNotNull(result.getSolution());
		int solution_count = 0;
		List<String> solution_list = new ArrayList<String>();
		while (result.isSuccess()) {
			solution_count += 1;
			List<Var> vars = result.getBindingVars();
			for (Var var: vars){
				solution_list.add(result.getVarValue(var.getName()).toString());
			}
			//System.out.println(solution.getBindingVars());
			//System.out.println(solution.getSolution().toString());
			if (engine.hasOpenAlternatives())
				result = engine.solveNext();
			else
				break;
		}
	}
	
	@Test
	public void test3() throws InvalidTheoryException, FileNotFoundException, IOException, MalformedGoalException, NoSolutionException, NoMoreSolutionException {
		String kbfolder = "E:/sadl/workspace-sadl/Mobius2.new/OwlModels";
		Prolog engine = new Prolog();
		engine.clearTheory();
		File plFilesFolder = new File(kbfolder);
		File[] files = plFilesFolder.listFiles(); 
		// first load prolog files and then owl/rdf files
		for (int i = 0; i < files.length; i++){
			String fn = files[i].getName();
			if (fn.endsWith(".pl") && 
					(fn.startsWith("PlanR") || fn.startsWith("plann") || fn.startsWith("pr") || fn.startsWith("r") || fn.startsWith("b"))) {
				System.out.println("Loading " + files[i].getAbsolutePath());
				try {
					engine.addTheory(createTheory(files[i].getAbsolutePath()));
				} catch (InvalidTheoryException e) {
					// TODO Auto-generated catch block
					System.err.println("Syntax error: " + e.getMessage());
				}
			}
		}
		
		SolveInfo result = engine.solve("preconditions(X,Y,Z).");
		System.out.println(result.toString());
		assertNotNull(result);
		assertNotNull(result.getSolution());
		int solution_count = 0;
		List<String> solution_list = new ArrayList<String>();
		while (result.isSuccess()) {
			solution_count += 1;
			List<Var> vars = result.getBindingVars();
			for (Var var: vars){
				solution_list.add(result.getVarValue(var.getName()).toString());
			}
			//System.out.println(solution.getBindingVars());
			//System.out.println(solution.getSolution().toString());
			if (engine.hasOpenAlternatives())
				result = engine.solveNext();
			else
				break;
		}
	}
	
	private Theory createTheory(String filename) throws InvalidTheoryException {
		return new Theory(":- consult('" + filename + "').");
	}

}
