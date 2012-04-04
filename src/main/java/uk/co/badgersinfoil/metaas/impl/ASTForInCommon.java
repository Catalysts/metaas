/*
 * ASTForInCommon.java
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

import uk.co.badgersinfoil.metaas.dom.Expression;
import uk.co.badgersinfoil.metaas.dom.StatementContainer;
import uk.co.badgersinfoil.metaas.impl.antlr.LinkedListTree;


/**
 * Common code implementing 'for-in' and 'for-each-in' loop behaviour.
 */
abstract class ASTForInCommon extends ContainerDelegate {

	private static final int INDEX_VAR = 0;
	private static final int INDEX_ITERATED = 1;
	private static final int INDEX_STATEMENT = 2;

	public ASTForInCommon(LinkedListTree ast) {
		super(ast);
	}

	public String getVarString() {
		return ASTUtils.stringifyNode(getChild(INDEX_VAR));
	}

	public String getIteratedString() {
		return ASTUtils.stringifyNode(iterated());
	}

	public Expression getIterated() {
		return ExpressionBuilder.build(iterated());
	}

	public void setVar(String expr) {
		LinkedListTree var = AS3FragmentParser.parseForInVar(expr);
		ast.setChildWithTokens(INDEX_VAR, var);
	}

	public void setIterated(String expr) {
		setIter(AS3FragmentParser.parseForInIterated(expr));
	}

	public void setIterated(Expression expr) {
		setIter(ast(expr));
	}

	private LinkedListTree getChild(int index) {
		return (LinkedListTree)ast.getChild(index);
	}

	protected StatementContainer getStatementContainer() {
		return new ASTStatementList(getChild(INDEX_STATEMENT));
	}
	private LinkedListTree iterated() {
		return getChild(INDEX_ITERATED);
	}
	private void setIter(LinkedListTree iterAST) {
		ast.setChildWithTokens(INDEX_ITERATED, iterAST);
	}
}