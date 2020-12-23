package com.ge.research.sadl.swi_prolog.plinterface;

import java.util.Hashtable;
import java.util.List;

public interface ISWIPrologServiceInterface {

	boolean addPlRules(String inRules);

	boolean clearPlRules();

	String getPlRules();

	boolean runPlQueryNoArgs(String url, String query, boolean defineQueryPred) throws Exception;

	List<Hashtable> runPlQuery(String url, String query, String target, boolean defineQueryPred) throws Exception;

	List<Hashtable> runPlQueryMultipleArgs(String url, String query, List<String> tList, boolean defineQueryPred)
			throws Exception;

}