# call subroutines

import sqlalchemy

from sqlalchemy import create_engine, Sequence
from sqlalchemy import Column, Integer, String
from sqlalchemy.ext.declarative import declarative_base


# specify method to call sql
engine = create_engine('mysql+pymysql://localhost/test', pool_recycle=5, echo=False)

# setup schema
Base = declarative_base()

class User(Base):
	__tablename__ = 'chemicals'

id = Column(Integer, Sequence('user_id_seq'), primary_key=True)
chemical = Column(String(100))
toxicity = Column(String(50))
mechanism = Column(String(10))
cas = Column(String(50))
listed = Column(String(50))
nsrl_madl = Column(String(50))

# spit out the entry if asked
def __repr__(self):
	return "<User(name='%s', fullname='%s', password='%s')>" % (self.name, self.fullname, self.password)
# create that table!
Base.metadata.create_all(engine)


# create a session to talk to SQL
from sqlalchemy.orm import sessionmaker
Session = sessionmaker(bind=engine)

session = Session()

# test adding some data
aldrin = Chem(chemical='aldrin', toxicity='cancer', mechanism='SQE', 
	cas='309-00-2', listed='1-Jul-88', nsrl_madl='0.04')
session.add(aldrin)

# stop talking to the server
session.commit()
