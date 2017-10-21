# --- Initial evolution

# --- !Ups

CREATE TABLE card_class (
  id serial primary key,
  name varchar(20),
  imgurl varchar(75)
);

# --- !Downs

DROP TABLE card_class;
