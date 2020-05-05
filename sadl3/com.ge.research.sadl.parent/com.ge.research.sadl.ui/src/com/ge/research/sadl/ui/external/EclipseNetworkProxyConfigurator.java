package com.ge.research.sadl.ui.external;

import java.util.Iterator;
import java.util.Map.Entry;
import java.util.Properties;

import org.eclipse.emf.common.EMFPlugin;

import com.ge.research.sadl.external.NetworkProxyConfigurator;

public class EclipseNetworkProxyConfigurator extends NetworkProxyConfigurator {

	@Override
	public void configureProxies() {
		Properties p = System.getProperties();
		Iterator<Object> pitr = p.keySet().iterator();
		while (pitr.hasNext()) {
			Object key = pitr.next();
			Object prop = p.get(key);
			// System.out.println("Key=" + key.toString() + ", value = " + prop.toString());
		}
		if (EMFPlugin.IS_ECLIPSE_RUNNING) {
			for (Entry<String, String> entry : new NetworkProxySettingsProvider().getConfigurations().entrySet()) {
				p.put(entry.getKey(), entry.getValue());
			}
		}
		System.setProperties(p);
	}

}
