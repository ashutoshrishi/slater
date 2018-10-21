extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;

use serde::{
    de::{self, Deserialize, Deserializer, MapAccess, Visitor},
    ser::{Serialize, SerializeStruct, Serializer},
};
use std::{fmt, slice};

#[derive(Debug, Default, Deserialize)]
pub struct Value {
    pub document: Document,
}

#[derive(Debug, Default, Deserialize)]
pub struct Document {
    pub nodes: Vec<Node>,
}

#[derive(Debug, Serialize)]
#[serde(untagged)]
pub enum Node {
    Document(Document),
    Block(Block),
    Inline(Inline),
    Text(Text),
    Unknown(String),
}

#[derive(Debug, Default, Deserialize)]
pub struct Block {
    pub nodes: Vec<Node>,
}

#[derive(Debug, Default, Deserialize)]
pub struct Inline {
    pub nodes: Vec<Node>,
}

#[derive(Debug, Default, Deserialize)]
pub struct Text {
    pub leaves: Vec<Leaf>,
}

#[derive(Debug, Deserialize)]
pub struct Leaf {
    pub text: String,
}

impl Value {
    pub fn new(document: Document) -> Value {
        Value { document }
    }

    pub fn leafs(&self) -> Leafs {
        Leafs::new(self)
    }
}

impl Document {
    pub fn add_node(&mut self, node: Node) {
        self.nodes.push(node);
    }
}

impl Block {
    pub fn add_node(&mut self, node: Node) {
        self.nodes.push(node);
    }
}

impl Inline {
    pub fn add_node(&mut self, node: Node) {
        self.nodes.push(node);
    }
}

impl Text {
    pub fn add_leaf(&mut self, leaf: Leaf) {
        self.leaves.push(leaf);
    }
}

impl Leaf {
    pub fn new(text: String) -> Leaf {
        Leaf { text }
    }
}

impl Serialize for Value {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut state = serializer.serialize_struct("Value", 2)?;
        state.serialize_field("object", "value")?;
        state.serialize_field("document", &self.document)?;
        state.end()
    }
}

impl Serialize for Document {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut state = serializer.serialize_struct("Document", 2)?;
        state.serialize_field("object", "document")?;
        state.serialize_field("nodes", &self.nodes)?;
        state.end()
    }
}

impl Serialize for Block {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut state = serializer.serialize_struct("Block", 2)?;
        state.serialize_field("object", "block")?;
        state.serialize_field("nodes", &self.nodes)?;
        state.end()
    }
}

impl Serialize for Inline {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut state = serializer.serialize_struct("Inline", 2)?;
        state.serialize_field("object", "inline")?;
        state.serialize_field("nodes", &self.nodes)?;
        state.end()
    }
}

impl Serialize for Text {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut state = serializer.serialize_struct("Text", 2)?;
        state.serialize_field("object", "text")?;
        state.serialize_field("leaves", &self.leaves)?;
        state.end()
    }
}

impl Serialize for Leaf {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut state = serializer.serialize_struct("Leaf", 2)?;
        state.serialize_field("object", "leaf")?;
        state.serialize_field("text", &self.text)?;
        state.end()
    }
}

impl<'de> Deserialize<'de> for Node {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Debug)]
        enum Field {
            Object,
            Nodes,
            Leaves,
            Unknown,
        };

        impl<'de> Deserialize<'de> for Field {
            fn deserialize<D>(deserializer: D) -> Result<Field, D::Error>
            where
                D: Deserializer<'de>,
            {
                #[derive(Debug)]
                struct FieldVisitor;

                impl<'de> Visitor<'de> for FieldVisitor {
                    type Value = Field;

                    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                        formatter.write_str("`object`, `nodes` or `leaves`")
                    }

                    fn visit_str<E>(self, value: &str) -> Result<Field, E>
                    where
                        E: de::Error,
                    {
                        match value {
                            "object" => Ok(Field::Object),
                            "nodes" => Ok(Field::Nodes),
                            "leaves" => Ok(Field::Leaves),
                            _ => Ok(Field::Unknown),
                        }
                    }
                }

                deserializer.deserialize_identifier(FieldVisitor)
            }
        }

        #[derive(Debug)]
        struct StructVisitor;

        impl<'de> Visitor<'de> for StructVisitor {
            type Value = Node;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct Node")
            }

            fn visit_map<V>(self, mut map: V) -> Result<Node, V::Error>
            where
                V: MapAccess<'de>,
            {
                let mut object = None;
                let mut nodes = None;
                let mut leaves = None;
                while let Some(key) = map.next_key()? {
                    match key {
                        Field::Object => {
                            if object.is_some() {
                                return Err(de::Error::duplicate_field("object"));
                            }
                            object = Some(map.next_value()?);
                        }
                        Field::Nodes => {
                            if nodes.is_some() {
                                return Err(de::Error::duplicate_field("nodes"));
                            }
                            nodes = Some(map.next_value()?);
                        }
                        Field::Leaves => {
                            if leaves.is_some() {
                                return Err(de::Error::duplicate_field("leaves"));
                            }
                            leaves = Some(map.next_value()?);
                        }
                        Field::Unknown => {
                            let _: serde_json::Value = map.next_value()?;
                        }
                    }
                }
                let object: String = object.ok_or_else(|| de::Error::missing_field("object"))?;
                match object.as_str() {
                    "document" => {
                        let nodes = nodes.ok_or_else(|| de::Error::missing_field("nodes"))?;
                        let document = Document { nodes };
                        Ok(Node::Document(document))
                    }
                    "block" => {
                        let nodes = nodes.ok_or_else(|| de::Error::missing_field("nodes"))?;
                        let block = Block { nodes };
                        Ok(Node::Block(block))
                    }
                    "inline" => {
                        let nodes = nodes.ok_or_else(|| de::Error::missing_field("nodes"))?;
                        let inline = Inline { nodes };
                        Ok(Node::Inline(inline))
                    }
                    "text" => {
                        let leaves = leaves.ok_or_else(|| de::Error::missing_field("leaves"))?;
                        let text = Text { leaves };
                        Ok(Node::Text(text))
                    }
                    _ => Ok(Node::Unknown(object)),
                }
            }
        }

        const FIELDS: &[&str] = &["object", "nodes", "leaves"];
        deserializer.deserialize_struct("Node", FIELDS, StructVisitor)
    }
}

pub struct Leafs<'a> {
    iter_tree_iter: IterTreeIter<'a>,
    curr_leaf_iter: Option<slice::Iter<'a, Leaf>>,
}

impl<'a> Leafs<'a> {
    pub fn new(value: &'a Value) -> Leafs<'a> {
        Leafs {
            iter_tree_iter: IterTreeIter::new(value.document.nodes.iter()),
            curr_leaf_iter: None,
        }
    }

    fn next_from_curr_leaf(&mut self) -> Option<&'a str> {
        self.curr_leaf_iter
            .as_mut()
            .and_then(|leaf_iter| leaf_iter.next())
            .map(|leaf| leaf.text.as_str())
    }

    fn next_from_node(&mut self) -> Option<&'a str> {
        self.curr_leaf_iter = self.iter_tree_iter.next();
        self.curr_leaf_iter.as_ref()?;

        self.next_from_curr_leaf()
    }
}

impl<'a> Iterator for Leafs<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<&'a str> {
        self.next_from_curr_leaf().or_else(|| self.next_from_node())
    }
}

struct IterTreeIter<'a> {
    iter_tree: Option<IterTree<'a>>,
}

struct IterTree<'a> {
    parent_iter: Option<Box<IterTree<'a>>>,
    curr_iter: slice::Iter<'a, Node>,
}

impl<'a> IterTreeIter<'a> {
    fn new(curr_iter: slice::Iter<'a, Node>) -> IterTreeIter<'a> {
        IterTreeIter {
            iter_tree: Some(IterTree {
                parent_iter: None,
                curr_iter,
            }),
        }
    }

    fn next_from_curr(&mut self) -> Option<slice::Iter<'a, Leaf>> {
        let node_iter = match self.iter_tree.as_mut()?.curr_iter.next()? {
            Node::Document(document) => document.nodes.iter(),
            Node::Block(block) => block.nodes.iter(),
            Node::Inline(inline) => inline.nodes.iter(),
            Node::Text(text) => return Some(text.leaves.iter()),
            Node::Unknown(_) => return None,
        };

        self.push_child_iter(node_iter);

        self.next_from_curr()
    }

    fn next_from_parent(&mut self) -> Option<slice::Iter<'a, Leaf>> {
        match self.iter_tree.take()?.parent_iter {
            Some(parent_iter) => {
                self.iter_tree = Some(*parent_iter);
            }
            None => {
                self.iter_tree = None;
                return None;
            }
        }

        self.next_from_curr()
    }

    fn push_child_iter(&mut self, child_iter: slice::Iter<'a, Node>) {
        let parent_iter_tree = self.iter_tree.take().map(Box::new);
        let new_iter_tree = IterTree {
            parent_iter: parent_iter_tree,
            curr_iter: child_iter,
        };
        self.iter_tree = Some(new_iter_tree);
    }
}

impl<'a> Iterator for IterTreeIter<'a> {
    type Item = slice::Iter<'a, Leaf>;

    fn next(&mut self) -> Option<slice::Iter<'a, Leaf>> {
        self.next_from_curr().or_else(|| self.next_from_parent())
    }
}

#[cfg(test)]
mod tests {
    extern crate serde_json;
    use super::*;

    #[test]
    fn debug_test() {
        let text = r#"{
  "object": "value",
  "document": {
    "object": "document",
    "data": {},
    "nodes": [
      {
        "object": "block",
        "type": "paragraph",
        "data": {},
        "nodes": [
          {
            "object": "text",
            "leaves": [
              {
                "object": "leaf",
                "text": "a",
                "marks": []
              }
            ]
          }
        ]
      },
      {
        "object": "block",
        "type": "paragraph",
        "data": {},
        "nodes": [
          {
            "object": "text",
            "leaves": [
              {
                "object": "leaf",
                "text": "",
                "marks": []
              }
            ]
          }
        ]
      },
      {
        "object": "block",
        "type": "bulleted-list",
        "data": {},
        "nodes": [
          {
            "object": "block",
            "type": "list-item",
            "data": {},
            "nodes": [
              {
                "object": "text",
                "leaves": [
                  {
                    "object": "leaf",
                    "text": "a",
                    "marks": []
                  }
                ]
              }
            ]
          },
          {
            "object": "block",
            "type": "list-item",
            "data": {},
            "nodes": [
              {
                "object": "text",
                "leaves": [
                  {
                    "object": "leaf",
                    "text": "b",
                    "marks": []
                  }
                ]
              }
            ]
          },
          {
            "object": "block",
            "type": "list-item",
            "data": {},
            "nodes": [
              {
                "object": "text",
                "leaves": [
                  {
                    "object": "leaf",
                    "text": "c",
                    "marks": []
                  }
                ]
              }
            ]
          }
        ]
      },
      {
        "object": "block",
        "type": "paragraph",
        "data": {},
        "nodes": [
          {
            "object": "text",
            "leaves": [
              {
                "object": "leaf",
                "text": "",
                "marks": []
              }
            ]
          }
        ]
      },
      {
        "object": "block",
        "type": "paragraph",
        "data": {},
        "nodes": [
          {
            "object": "text",
            "leaves": [
              {
                "object": "leaf",
                "text": "d",
                "marks": []
              }
            ]
          }
        ]
      },
      {
        "object": "block",
        "type": "numbered-list",
        "data": {},
        "nodes": [
          {
            "object": "block",
            "type": "list-item",
            "data": {},
            "nodes": [
              {
                "object": "text",
                "leaves": [
                  {
                    "object": "leaf",
                    "text": "e",
                    "marks": []
                  }
                ]
              }
            ]
          },
          {
            "object": "block",
            "type": "list-item",
            "data": {},
            "nodes": [
              {
                "object": "text",
                "leaves": [
                  {
                    "object": "leaf",
                    "text": "f",
                    "marks": []
                  }
                ]
              }
            ]
          },
          {
            "object": "block",
            "type": "list-item",
            "data": {},
            "nodes": [
              {
                "object": "text",
                "leaves": [
                  {
                    "object": "leaf",
                    "text": "g",
                    "marks": []
                  }
                ]
              }
            ]
          }
        ]
      }
    ]
  }
}"#;

        let value: Result<Value, _> = serde_json::from_str(text);
        assert!(value.is_ok());
    }

    #[test]
    pub fn smoke_test() {
        let mut document = Document::default();
        {
            let mut block = Block::default();
            {
                let mut text = Text::default();
                {
                    let leaf = Leaf::new("sss".into());
                    text.add_leaf(leaf);
                }
                block.add_node(Node::Text(text));
            }
            document.add_node(Node::Block(block));
        }

        let value1 = Value::new(document);

        let value1_json = serde_json::to_string(&value1).unwrap();
        assert_eq!(value1_json, r#"{"object":"value","document":{"object":"document","nodes":[{"object":"block","nodes":[{"object":"text","leaves":[{"object":"leaf","text":"sss"}]}]}]}}"#);

        let value2: Value = serde_json::from_str(&value1_json).unwrap();
        let value2_json = serde_json::to_string(&value2).unwrap();
        assert_eq!(value1_json, value2_json);
    }

    #[test]
    pub fn smoke_visit_leafs() {
        let mut document = Document::default();
        {
            {
                let mut block = Block::default();
                {
                    let mut text = Text::default();
                    text.add_leaf(Leaf::new("leaf 1".into()));
                    text.add_leaf(Leaf::new("leaf 2".into()));
                    block.add_node(Node::Text(text));
                }
                document.add_node(Node::Block(block));
            }
            {
                let mut block = Block::default();
                {
                    let mut text = Text::default();
                    text.add_leaf(Leaf::new("leaf 3".into()));
                    text.add_leaf(Leaf::new("leaf 4".into()));
                    block.add_node(Node::Text(text));
                }
                document.add_node(Node::Block(block))
            }
            {
                let mut block = Block::default();
                {
                    let mut block_inner = Block::default();
                    {
                        let mut text = Text::default();
                        text.add_leaf(Leaf::new("leaf 5".into()));
                        text.add_leaf(Leaf::new("leaf 6".into()));
                        block_inner.add_node(Node::Text(text));
                    }
                    block.add_node(Node::Block(block_inner));
                }
                document.add_node(Node::Block(block))
            }
        }
        let value = Value::new(document);
        let mut leafs = value.leafs();

        assert_eq!(Some("leaf 1"), leafs.next());
        assert_eq!(Some("leaf 2"), leafs.next());
        assert_eq!(Some("leaf 3"), leafs.next());
        assert_eq!(Some("leaf 4"), leafs.next());
        assert_eq!(Some("leaf 5"), leafs.next());
        assert_eq!(Some("leaf 6"), leafs.next());
        assert_eq!(None, leafs.next());
        assert_eq!(None, leafs.next());
    }
}
