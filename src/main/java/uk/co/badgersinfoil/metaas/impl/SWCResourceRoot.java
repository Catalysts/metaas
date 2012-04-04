/*
 * SWCResourceRoot.java
 * 
 * Copyright (c) 2007 David Holroyd
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.co.badgersinfoil.metaas.impl;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;


public class SWCResourceRoot implements ResourceRoot {
	
	private List qnames = null;
	
	private static final String CATALOG_FILENAME = "catalog.xml";

	public SWCResourceRoot(String filename) throws IOException {
		ZipFile zip = new ZipFile(filename);
		try {
			ZipEntry entry = zip.getEntry(CATALOG_FILENAME);
			if (entry == null) {
				throw new IllegalArgumentException("No "+CATALOG_FILENAME+" in swc: "+filename);
			}
			qnames = readCatalog(zip.getInputStream(entry));
		} catch (ParserConfigurationException e) {
			throw new IOException(e.toString());
		} catch (SAXException e) {
			throw new IOException(e.toString());
		} catch (XPathExpressionException e) {
			throw new IOException(e.toString());
		} finally {
			zip.close();
		}
	}

	private List readCatalog(InputStream in) throws ParserConfigurationException, SAXException, IOException, XPathExpressionException {
		Document doc = loadDoc(in);
		XPathFactory fact = XPathFactory.newInstance();
		XPath xpath = fact.newXPath();
		// TODO: work out how to provide namespace
		// "http://www.adobe.com/flash/swccatalog/9" to the xpath
		NodeList list = (NodeList)xpath.evaluate("/swc/libraries/library/script/def", doc, XPathConstants.NODESET);
		List result = new ArrayList();
		for (int i=0; i<list.getLength(); i++) {
			Element def = (Element)list.item(i);
			String defined = def.getAttribute("id");
			result.add(toQName(defined));
		}
		return result;
	}

	private ASQName toQName(String def) {
		int pos = def.indexOf(':');
		if (pos > 0) {
			return new ASQName(def.substring(0, pos), def.substring(pos+1));
		}
		return new ASQName(null, def);
	}

	private Document loadDoc(InputStream in) throws ParserConfigurationException, SAXException, IOException {
		DocumentBuilderFactory fact = DocumentBuilderFactory.newInstance();
		DocumentBuilder builder = fact.newDocumentBuilder();
		InputSource is = new InputSource(in);
		is.setSystemId(CATALOG_FILENAME);
		return builder.parse(is);
	}

	public List getDefinitionQNames() {
		return qnames;
	}
}
