package com.ge.research.sadl.scoping;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.scoping.IScope;

public class TestScopeProvider {
	
	static class TestScopeAdapter extends AdapterImpl  {
		IScope scope;
		
		@Override
		public boolean isAdapterForType(Object type) {
			return IScope.class == type;
		}
	}
	
	public static void registerResource(Resource resource) {
		TestScopeAdapter a = findAdapter(resource);
		if (a == null) {
			a = new TestScopeAdapter();
			resource.eAdapters().add(a);
		}
	}
	
	public static void attach(Resource resource, IScope scope) {
		TestScopeAdapter adapter = findAdapter(resource);
		if (adapter == null) {
			adapter = new TestScopeAdapter();
			resource.eAdapters().add(adapter);
		}
		adapter.scope = scope;
	}
		
	public static TestScopeAdapter findAdapter(Resource resource) {
		if (resource != null) {
			for (Adapter a : resource.eAdapters()) {
				if (a instanceof TestScopeAdapter) {
					return ((TestScopeAdapter) a);
				}
			}
		}
		return null;
	}
	
	public static IScope find(Resource resource) {
		TestScopeAdapter a = findAdapter(resource);
		if (a != null) {
			return a.scope;
		}
		return null;
	}
	
}
