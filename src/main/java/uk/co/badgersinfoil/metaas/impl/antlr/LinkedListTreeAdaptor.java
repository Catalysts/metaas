/*
 * LinkedListTreeAdaptor.java
 * 
 * Copyright (c) 2006 David Holroyd
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

package uk.co.badgersinfoil.metaas.impl.antlr;

import org.antlr.runtime.CommonToken;
import org.antlr.runtime.Token;
import org.antlr.runtime.tree.BaseTreeAdaptor;
import org.antlr.runtime.tree.Tree;


public class LinkedListTreeAdaptor extends BaseTreeAdaptor {
	private Factory factory = new Factory() {
		private BasicListUpdateDelegate delegate = new BasicListUpdateDelegate();
		public TreeTokenListUpdateDelegate create(LinkedListTree payload) {
			return delegate;
		}
	};

	public void setFactory(Factory factory) {
		this.factory = factory;
	}

	public Token createToken(int tokenType, String text) {
		return new CommonToken(tokenType, text);
	}

	public Token createToken(Token fromToken) {
		return new CommonToken(fromToken);
	}

	public Object create(Token payload) {
		LinkedListTree result = new LinkedListTree(payload);
		result.setTokenListUpdater(factory.create(result));
		if (payload instanceof LinkedListToken) {
			result.setStartToken((LinkedListToken)payload);
			result.setStopToken((LinkedListToken)payload);
		}
		return result;
	}

	public Object dupNode(Object treeNode) {
		return ((Tree)treeNode).dupNode();
	}

	/**
	 * Only works with LinkedListTree nodes
	 */
	public void setTokenBoundaries(Object t, Token startToken, Token stopToken) {
		if ( t==null ) {
			return;
		}
		LinkedListTree tree = (LinkedListTree)t;
		tree.setStartToken((LinkedListToken)startToken);
		tree.setStopToken((LinkedListToken)stopToken);
	}

	public int getTokenStartIndex(Object arg0) {
		// TODO: what's appropriate here?
		return -1;
	}

	public int getTokenStopIndex(Object arg0) {
		// TODO: what's appropriate here?
		return -1;
	}

	public String getText(Object t) {
		return ((Tree)t).getText();
	}

	public int getType(Object t) {
		return ((Tree)t).getType();
	}

	public interface Factory {
		TreeTokenListUpdateDelegate create(LinkedListTree payload);
	}

	public Token getToken(Object t) {
		return ((LinkedListTree)t).getToken();
	}
}