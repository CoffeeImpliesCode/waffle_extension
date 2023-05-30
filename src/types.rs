use std::collections::HashSet;

#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct TypeId(pub u64);
impl TypeId {
    pub const UNIT: TypeId = TypeId(0);
    pub const INTEGER: TypeId = TypeId(1);
    pub const STRING: TypeId = TypeId(2);
    pub const BOOL: TypeId = TypeId(3);
    pub const ATOM: TypeId = TypeId(4);
}

#[derive(Debug, Clone, Hash)]
pub enum Type {
    Any,
    Unit,
    Integer,
    String,
    Bool,
    Atom,
    T(TypeId),
    Tuple(Vec<Type>),
    Function(FunctionType<Type>),
}

impl Type {
    pub fn new_any() -> Type {
        Type::Any
    }

    pub fn new_unit() -> Type {
        Type::Unit
    }

    pub fn new_integer() -> Type {
        Type::Integer
    }

    pub fn new_t(id: TypeId) -> Type {
        Type::T(id)
    }

    pub fn new_tuple(elements: &[Type]) -> Type {
        Type::Tuple(elements.into())
    }

    pub fn new_function(function: FunctionType) -> Type {
        Type::Function(function)
    }

    pub fn unit(&self) -> () {
        match self {
            Type::Unit => (),
            Type::T(id) if *id == TypeId::UNIT => (),
            _ => panic!("Error: type is not Unit."),
        }
    }

    pub fn get_unit(&self) -> Result<(), ()> {
        match self {
            Type::Unit => Ok(()),
            Type::T(id) if *id == TypeId::UNIT => Ok(()),
            _ => Err(()),
        }
    }

    pub fn integer(&self) -> () {
        match self {
            Type::Integer => (),
            Type::T(id) if *id == TypeId::INTEGER => (),
            _ => panic!("Error: type is not Integer"),
        }
    }

    pub fn get_integer(&self) -> Result<(), ()> {
        match self {
            Type::Integer => Ok(()),
            Type::T(id) if *id == TypeId::INTEGER => Ok(()),
            _ => Err(()),
        }
    }

    pub fn t(&self) -> TypeId {
        match self {
            Type::T(id) => *id,
            Type::Unit => TypeId::UNIT,
            Type::Integer => TypeId::INTEGER,
            _ => panic!("Error: type is not T"),
        }
    }

    pub fn get_t(&self) -> Result<TypeId, ()> {
        match self {
            Type::Unit => Ok(TypeId::UNIT),
            Type::Integer => Ok(TypeId::INTEGER),
            Type::T(id) => Ok(*id),
            _ => Err(()),
        }
    }

    pub fn function(&self) -> FunctionType {
        match self {
            Type::Function(function) => function.clone(),
            _ => panic!("Error: type is not a function"),
        }
    }

    pub fn get_function(&self) -> Result<FunctionType, ()> {
        match self {
            Type::Function(function) => Ok(function.clone()),
            _ => Err(()),
        }
    }

    pub fn arrow(self, to: Type) -> Type {
        Type::Function(FunctionType::new(self, to))
    }
}

impl Default for Type {
    fn default() -> Type {
        Type::Unit
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Type) -> bool {
        use Type::*;
        match (self, other) {
            (Unit, Unit) => true,
            (Integer, Integer) => true,
            (Unit, T(TypeId::UNIT)) => true,
            (T(TypeId::UNIT), Unit) => true,
            (Integer, T(TypeId::INTEGER)) => true,
            (T(TypeId::INTEGER), Integer) => true,
            (T(a), T(b)) => a == b,
            (Tuple(xs), Tuple(ys)) => xs
                .iter()
                .zip(ys.iter())
                .map(|(a, b)| a == b)
                .fold(true, |a, b| a && b),
            (Function(a), Function(b)) => a == b,
            (_, _) => false,
        }
    }
}

impl Eq for Type {}

impl From<&[Type]> for Type {
    fn from(from: &[Type]) -> Type {
        Type::new_tuple(from)
    }
}

impl From<&Vec<Type>> for Type {
    fn from(from: &Vec<Type>) -> Type {
        Type::new_tuple(from)
    }
}

impl From<Vec<Type>> for Type {
    fn from(from: Vec<Type>) -> Type {
        Type::new_tuple(&from[..])
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Any => write!(f, "*"),
            Type::Unit => write!(f, "Unit"),
            Type::Integer => write!(f, "Int"),
            Type::Bool => write!(f, "Bool"),
            Type::String => write!(f, "String"),
            Type::Atom => write!(f, "Atom"),
            Type::T(id) => write!(f, "#{}", id.0),
            Type::Tuple(ts) => match &ts[..] {
                [] => write!(f, "()"),
                [a] => write!(f, "({})", a),
                [a, bs @ ..] => {
                    write!(f, "({}", a)?;
                    for t in bs {
                        write!(f, " {}", t)?;
                    }
                    write!(f, ")")
                }
            },
            Type::Function(fun) => write!(f, "{}", fun),
            _ => write!(f, "<Display: unimplemented>"),
        }
    }
}

#[derive(Debug, Clone, Hash)]
pub struct FunctionType<T = Type> {
    from: Box<T>,
    to: Box<T>,
}

impl<T> FunctionType<T> {
    pub fn new(from: T, to: T) -> FunctionType<T> {
        FunctionType::<T> {
            from: Box::new(from),
            to: Box::new(to),
        }
    }

    pub fn from(&self) -> &T {
        &self.from
    }

    pub fn from_mut(&mut self) -> &mut T {
        &mut self.from
    }

    pub fn to(&self) -> &T {
        &self.to
    }

    pub fn to_mut(&mut self) -> &mut T {
        &mut self.to
    }

    pub fn into_from(self) -> T {
        *self.from
    }

    pub fn into_to(self) -> T {
        *self.to
    }
}

impl Into<Type> for FunctionType<Type> {
    fn into(self) -> Type {
        Type::Function(self)
    }
}

// impl Into<TypeExpr> for FunctionType<TypeExpr> {
//     fn into(self) -> TypeExpr {
//         TypeExpr::Function(self)
//     }
// }

impl PartialEq for FunctionType {
    fn eq(&self, other: &FunctionType) -> bool {
        self.from == other.from && self.to == other.to
    }
}

impl Eq for FunctionType {}

impl<T: std::fmt::Display> std::fmt::Display for FunctionType<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} → {})", self.from, self.to)
    }
}

// #[derive(Debug, Clone)]
// pub enum TypeExpr {
//     Unit,
//     Integer,
//     Variable(String),
//     Function(FunctionType<TypeExpr>),
// }

// impl TypeExpr {
//     pub fn new_unit() -> TypeExpr {
//         TypeExpr::Unit
//     }

//     pub fn new_integer() -> TypeExpr {
//         TypeExpr::Integer
//     }

//     pub fn new_variable(name: impl Into<String>) -> TypeExpr {
//         TypeExpr::Variable(name.into())
//     }

//     pub fn new_function(from: TypeExpr, to: TypeExpr) -> TypeExpr {
//         TypeExpr::Function(FunctionType::<TypeExpr>::new(from, to))
//     }

//     pub fn arrow(self, to: TypeExpr) -> TypeExpr {
//         TypeExpr::Function(FunctionType::<TypeExpr>::new(self, to))
//     }

//     pub fn substitute(&mut self, subst: (&str, &TypeExpr)) {
//         match self {
//             TypeExpr::Variable(v) if v == subst.0 => {
//                 *self = subst.1.clone();
//             }
//             TypeExpr::Function(f) => {
//                 f.from.substitute(subst.clone());
//                 f.to.substitute(subst);
//             }
//             _ => {}
//         }
//     }

//     fn substituted(&self, subst: (&str, &TypeExpr)) -> TypeExpr {
//         let mut new = self.clone();
//         new.substitute(subst);
//         new
//     }

//     fn variables(&self) -> HashSet<String> {
//         let mut vars: HashSet<String> = HashSet::new();

//         fn find(expr: &TypeExpr, into: &mut HashSet<String>) {
//             match expr {
//                 TypeExpr::Variable(var) => {
//                     if !into.contains(var) {
//                         into.insert(var.to_string());
//                     }
//                 }
//                 TypeExpr::Function(fun) => {
//                     find(&fun.from, into);
//                     find(&fun.to, into);
//                 }
//                 _ => {}
//             }
//         }

//         find(self, &mut vars);
//         vars
//     }

//     fn contains_variables(&self) -> bool {
//         match self {
//             TypeExpr::Variable(_) => true,
//             TypeExpr::Function(fun) => fun.from.contains_variables() || fun.to.contains_variables(),
//             _ => false,
//         }
//     }

//     fn contains(&self, var: &str) -> bool {
//         match self {
//             TypeExpr::Variable(v) if v == var => true,
//             TypeExpr::Function(fun) => fun.from.contains(var) || fun.to.contains(var),
//             _ => false,
//         }
//     }
// }
