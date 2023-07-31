use json::object;
use json::JsonValue;

use crate::compiler::identifier::Identifier;
use crate::compiler::identifier::VariableIdentifier;
use crate::compiler::types::TypeId;

use super::expression::*;
use super::statement::*;
use super::visitor::ExprVisitor;
use super::visitor::ProgramVisitor;
use super::visitor::StmtVisitor;
use super::AST;

impl From<Identifier> for JsonValue {
  fn from(value: Identifier) -> Self {
    JsonValue::String(format!("{value:?}"))
  }
}

impl From<VariableIdentifier> for JsonValue {
  fn from(value: VariableIdentifier) -> Self {
    JsonValue::String(format!("{value:?}"))
  }
}

impl From<TypeId> for JsonValue {
  fn from(value: TypeId) -> Self {
    JsonValue::String(format!("{}", value.0))
  }
}

pub struct ASTJSONPrinter {}

impl ExprVisitor<JsonValue> for ASTJSONPrinter {
  fn visit_literal_string(&mut self, ast: &AST, literal_string: &expr::LiteralString) -> JsonValue {
    object! {
      "LiteralString": {
        "value": literal_string.handle.get(ast)
      }
    }
  }

  fn visit_literal_number(&mut self, ast: &AST, literal_number: &expr::LiteralNumber) -> JsonValue {
    object! {
      "LiteralNumber": {
        "value": literal_number.value
      }
    }
  }

  fn visit_literal_bool(&mut self, ast: &AST, literal_bool: &expr::LiteralBool) -> JsonValue {
    object! {
      "LiteralNumber": {
        "value": literal_bool.value
      }
    }
  }

  fn visit_paren(&mut self, ast: &AST, paren: &expr::Paren) -> JsonValue {
    object! {
      "Paren": {
        "expr": self.visit_expr(ast, paren.inner)
      }
    }
  }

  fn visit_binary(&mut self, ast: &AST, binary: &expr::Binary) -> JsonValue {
    object! {
      "Binary": {
        "operator": format!("{}", binary.operator),
        "left": self.visit_expr(ast, binary.left),
        "right": self.visit_expr(ast, binary.right),
      }
    }
  }

  fn visit_logical(&mut self, ast: &AST, logical: &expr::Logical) -> JsonValue {
    object! {
      "Logical": {
        "operator": format!("{}", logical.operator),
        "left": self.visit_expr(ast, logical.left),
        "right": self.visit_expr(ast, logical.right),
      }
    }
  }

  fn visit_unary(&mut self, ast: &AST, unary: &expr::Unary) -> JsonValue {
    object! {
      "Unary": {
        "operator": format!("{}", unary.operator),
        "right": self.visit_expr(ast, unary.right),
      }
    }
  }

  fn visit_id(&mut self, ast: &AST, id: &expr::Id) -> JsonValue {
    object! {
      "Variable": {
        id: id.id
      }
    }
  }

  fn visit_assignment(&mut self, ast: &AST, assignment: &expr::Assignment) -> JsonValue {
    object! {
      "Assignment": {
        "id": assignment.id,
        "value": self.visit_expr(ast, assignment.value)
      }
    }
  }

  fn visit_lambda(&mut self, ast: &AST, lambda: &expr::Lambda) -> JsonValue {
    let body = lambda
      .body
      .iter()
      .map(|&stmt| self.visit_stmt(ast, stmt))
      .collect::<Vec<_>>();
    object! {
      "Lambda": {
        "captures": lambda.captures.as_slice(),
        "function_type_id": lambda.function_type_id,
        "return_type": lambda.return_type,
        "parameter_types": lambda.parameter_types.as_slice(),
        "body": body
      }
    }
  }

  fn visit_fn_call(&mut self, ast: &AST, fn_call: &expr::FnCall) -> JsonValue {
    let arguments = fn_call
      .arguments
      .iter()
      .map(|&e| self.visit_expr(ast, e))
      .collect::<Vec<_>>();
    object! {
      "FnCall": {
        "function": self.visit_expr(ast, fn_call.func),
        "arguments": arguments
      }
    }
  }

  fn visit_dot(&mut self, ast: &AST, dot: &expr::Dot) -> JsonValue {
    object! {
      "Dot": {
        "lhs": self.visit_expr(ast, dot.lhs),
        "rhs_name": dot.rhs_name.get(ast),
        "rhs_id": dot.rhs_id
      }
    }
  }

  fn visit_set(&mut self, ast: &AST, set: &expr::Set) -> JsonValue {
    object! {
      "Set": {
        "object": self.visit_expr(ast, set.object),
        "member_name": set.member_name.get(ast),
        "value": self.visit_expr(ast, set.value),
      }
    }
  }
}

impl StmtVisitor<JsonValue> for ASTJSONPrinter {
  fn visit_var_decl(&mut self, ast: &AST, var_decl: &stmt::VarDecl) -> JsonValue {
    let init_expr = if let Some(expr) = var_decl.expression {
      self.visit_expr(ast, expr)
    } else {
      JsonValue::Null
    };
    object! {
      "VarDecl": {
        "id": var_decl.identifier,
        "type": format!("{:?}", var_decl.var_type),
        "init expr": init_expr
      }
    }
  }

  fn visit_stmt_expr(&mut self, ast: &AST, expr: &stmt::StmtExpr) -> JsonValue {
    object! {
      "Expr": self.visit_expr(ast, expr.expr)
    }
  }

  fn visit_block(&mut self, ast: &AST, block: &stmt::Block) -> JsonValue {
    let stmts = block
      .statements
      .iter()
      .map(|&s| self.visit_stmt(ast, s))
      .collect::<Vec<_>>();
    object! {
      "Block": {
        "locals": format!("{}", block.locals),
        "statements": stmts
      }
    }
  }

  fn visit_if_branch(&mut self, ast: &AST, if_branch: &stmt::IfBranch) -> JsonValue {
    let else_branch = if let Some(stmt) = if_branch.else_branch {
      self.visit_stmt(ast, stmt)
    } else {
      JsonValue::Null
    };
    object! {
      "IfBranch": {
        "condition": self.visit_expr(ast, if_branch.condition),
        "true branch": self.visit_stmt(ast, if_branch.true_branch),
        "else branch": else_branch
      }
    }
  }

  fn visit_while(&mut self, ast: &AST, while_stmt: &stmt::While) -> JsonValue {
    object! {
      "While": {
        "condition": self.visit_expr(ast, while_stmt.condition),
        "body": self.visit_stmt(ast, while_stmt.loop_body)
      }
    }
  }

  fn visit_function_definition(
    &mut self,
    ast: &AST,
    function_definition: &stmt::FunctionDefinition,
  ) -> JsonValue {
    let body = function_definition
      .body
      .iter()
      .map(|&stmt| self.visit_stmt(ast, stmt))
      .collect::<Vec<_>>();
    object! {
      "FunctionDefinition": {
        "id": function_definition.id,
        "captures": function_definition.captures.as_slice(),
        "parameter types": function_definition.parameter_types.as_slice(),
        "function type": function_definition.fn_type,
        "return type": function_definition.return_type,
        "body": body
      }
    }
  }

  fn visit_function_declaration(
    &mut self,
    ast: &AST,
    function_declaration: &stmt::FunctionDeclaration,
  ) -> JsonValue {
    object! {
      "FunctionDeclaration": {
        "id": function_declaration.id,
        "function type": function_declaration.fn_type,
        "parameter types": function_declaration.parameter_types.as_slice(),
        "return type": function_declaration.return_type,
      }
    }
  }

  fn visit_extern_function(
    &mut self,
    ast: &AST,
    extern_function: &stmt::ExternFunction,
  ) -> JsonValue {
    object! {
      "ExternalFunction": {
        "identifier": format!("{:?}", extern_function.identifier),
        "function type": extern_function.fn_type,
      }
    }
  }

  fn visit_break(&mut self, ast: &AST, break_node: &stmt::Break) -> JsonValue {
    JsonValue::String("Break".to_string())
  }

  fn visit_return(&mut self, ast: &AST, return_stmt: &stmt::Return) -> JsonValue {
    let ret_expr = if let Some(expr) = return_stmt.expr {
      self.visit_expr(ast, expr)
    } else {
      JsonValue::Null
    };
    object! {
      "Return": {
        "expr": ret_expr
      }
    }
  }

  fn visit_struct(&mut self, ast: &AST, struct_stmt: &stmt::Struct) -> JsonValue {
    let member_names = struct_stmt
      .member_names
      .iter()
      .map(|handle| handle.get(ast).to_string())
      .collect::<Vec<String>>();
    object! {
      "Struct": {
        "id": format!("{:?}", struct_stmt.id),
        "member_names": member_names,
        "member_types": struct_stmt.member_types.as_slice()
      }
    }
  }

  fn visit_import(&mut self, ast: &AST, import: &stmt::Import) -> JsonValue {
    object! {
      "Import": {
        "module id": format!("{}", import.module_id.0)
      }
    }
  }
}

impl ASTJSONPrinter {
  pub fn print_to_string(ast: &AST) -> String {
    let program = ProgramVisitor::new(ASTJSONPrinter {}, ast).collect::<Vec<_>>();
    json::stringify_pretty(program, 1)
  }

  pub fn to_json(ast: &AST) -> JsonValue {
    let program = ProgramVisitor::new(ASTJSONPrinter {}, ast).collect::<Vec<_>>();
    JsonValue::Array(program)
  }
}
