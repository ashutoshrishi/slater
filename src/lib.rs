extern crate serde;
#[macro_use]
extern crate serde_derive;

use serde::{
    de::{self, Deserialize, Deserializer, MapAccess, Unexpected, Visitor},
    ser::{Serialize, SerializeStruct, Serializer},
};
use std::fmt;

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
}

#[derive(Debug, Deserialize)]
pub struct Block {
    pub nodes: Vec<Node>,
}

#[derive(Debug, Deserialize)]
pub struct Inline {
    pub nodes: Vec<Node>,
}

#[derive(Debug, Deserialize)]
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
}

impl Document {
    pub fn new() -> Document {
        Document { nodes: Vec::new() }
    }

    pub fn add_node(&mut self, node: Node) {
        self.nodes.push(node);
    }
}

impl Block {
    pub fn new() -> Block {
        Block { nodes: Vec::new() }
    }

    pub fn add_node(&mut self, node: Node) {
        self.nodes.push(node);
    }
}

impl Inline {
    pub fn new() -> Inline {
        Inline { nodes: Vec::new() }
    }

    pub fn add_node(&mut self, node: Node) {
        self.nodes.push(node);
    }
}

impl Text {
    pub fn new() -> Text {
        Text { leaves: Vec::new() }
    }

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
        enum Field {
            Object,
            Nodes,
            Leaves,
        };

        impl<'de> Deserialize<'de> for Field {
            fn deserialize<D>(deserializer: D) -> Result<Field, D::Error>
            where
                D: Deserializer<'de>,
            {
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
                            _ => Err(de::Error::unknown_field(value, FIELDS)),
                        }
                    }
                }

                deserializer.deserialize_identifier(FieldVisitor)
            }
        }

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
                    _ => return Err(de::Error::invalid_type(Unexpected::Str(&object), &self)),
                }
            }
        }

        const FIELDS: &'static [&'static str] = &["object", "nodes", "leaves"];
        deserializer.deserialize_struct("Node", FIELDS, StructVisitor)
    }
}

#[cfg(test)]
mod tests {
    extern crate serde_json;
    use super::*;

    #[test]
    pub fn smoke_test() {
        let mut document = Document::new();
        {
            let mut block = Block::new();
            {
                let mut text = Text::new();
                {
                    let leaf = Leaf::new("sss".into());
                    text.add_leaf(leaf);
                }
                block.add_node(Node::Text(text));
            }
            document.add_node(Node::Block(block));
        }

        let value1 = Value::new(document);
        println!("value1: {:?}", value1);

        let value1_json = serde_json::to_string(&value1).unwrap();
        assert_eq!(value1_json, r#"{"object":"value","document":{"object":"document","nodes":[{"object":"block","nodes":[{"object":"text","leaves":[{"object":"leaf","text":"sss"}]}]}]}}"#);

        let value2: Value = serde_json::from_str(&value1_json).unwrap();
        let value2_json = serde_json::to_string(&value2).unwrap();
        assert_eq!(value1_json, value2_json);
    }
}
