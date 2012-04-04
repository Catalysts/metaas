/*
 * ASTASDoWhileStatement.java
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

import org.asdt.core.internal.antlr.AS3Parser;
import uk.co.badgersinfoil.metaas.dom.ASDoWhileStatement;
import uk.co.badgersinfoil.metaas.dom.Expression;
import uk.co.badgersinfoil.metaas.dom.StatementContainer;
import uk.co.badgersinfoil.metaas.impl.antlr.LinkedListTree;


public class ASTASDoWhileStatement extends ContainerDelegate implements ASDoWhileStatement {

	private static final int INDEX_STATEMENT = 0;
	private static final int INDEX_CONDITION = 1;

	public ASTASDoWhileStatement(LinkedListTree ast) {
		super(ast);
		ASTUtils.assertAS3TokenTypeIs(AS3Parser.DO, ast.getType());
	}

	private LinkedListTree getChild(int index) {
		return (LinkedListTree)ast.getChild(index);
	}
	private LinkedListTree condition() {
		return getChild(INDEX_CONDITION);
	}
	protected StatementContainer getStatementContainer() {
		return new ASTStatementList(getChild(INDEX_STATEMENT));
	}

	public String getConditionString() {
		return ASTUtils.stringifyNode(condition().getFirstChild());
	}

	public Expression getCondition() {
		return ExpressionBuilder.build(condition().getFirstChild());
	}

	public void setCondition(String expr) {
		LinkedListTree cond = AS3FragmentParser.parseCondition(expr);
		ast.setChildWithTokens(INDEX_CONDITION, cond);
	}

	public void setCondition(Expression expr) {
		condition().setChildWithTokens(0, ast(expr));
	}
}
