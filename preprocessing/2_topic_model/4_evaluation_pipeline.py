import pickle
import pandas as pd
import numpy as np
from sklearn.decomposition import PCA
from sklearn.feature_extraction.text import CountVectorizer
from umap import UMAP
from sentence_transformers import SentenceTransformer
from hdbscan import HDBSCAN
from bertopic import BERTopic
import os
import logging
import sys
from datetime import datetime
from threadpoolctl import threadpool_limits
from nltk.corpus import stopwords
from bertopic.vectorizers import ClassTfidfTransformer
from bertopic.representation import KeyBERTInspired
from sklearn.metrics.pairwise import cosine_similarity
import string
from octis.evaluation_metrics.coherence_metrics import Coherence
from octis.evaluation_metrics.diversity_metrics import TopicDiversity
import openpyxl


class EvaluationPipeline:
    def __init__(self, data_path='latam_text_topics_3title.txt', topic_model_path='../job_outputs/bertopic_output_20240302_195213/topic_model/'):
        self.data_path = data_path
        self.topic_model_path = topic_model_path
        self.df = None
        self.texts = None
        self.sentence_model = None
        self.topic_model = None
        self.hierarchical_topics = None
        self.distance_matrix = None
        self.labels = None
        self.cv = None
        self.dict = None
        self.dv = None
        self.diversity = None
        self.cohrence = None
        self.params_df = None
        self.outlier_prop_row = None
        self.coherence_row = None
        self.diversity_row = None
        self.corpus_info = None

        # Initialize logger
        self.logger = logging.getLogger(__name__)
        self.logger.setLevel(logging.INFO)
        logging.getLogger('gensim.topic_coherence.text_analysis').setLevel(logging.ERROR)
        logging.getLogger('gensim.corpora.dictionary').setLevel(logging.ERROR)
        formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')

        # Log to console
        ch = logging.StreamHandler()
        ch.setLevel(logging.INFO)
        ch.setFormatter(formatter)
        self.logger.addHandler(ch)

        # Log to file
        self.log_folder = None
        self.create_output_folder(topic_model_path)  # Create output folder
        log_file = os.path.join(self.log_folder, 'pipeline.log')  # Define log file path
        fh = logging.FileHandler(log_file)  # File handler for logging to file
        fh.setLevel(logging.INFO)
        fh.setFormatter(formatter)
        self.logger.addHandler(fh)  # Add file handler to logger

    def load_data(self):
        try:
            self.logger.info("Loading data from: {}".format(self.data_path))
            self.df = pd.read_csv(self.data_path, delimiter='\t')
            self.texts = self.df.text.values
        except Exception as e:
            self.logger.error("Error loading data: {}".format(e))

    def load_model(self):
        try:
            self.logger.info("Loading model from: {}".format(self.topic_model_path))
            self.sentence_model = SentenceTransformer("paraphrase-multilingual-MiniLM-L12-v2")
            self.topic_model = BERTopic.load(self.topic_model_path,embedding_model=self.sentence_model)
        except Exception as e:
            self.logger.error("Error loading model: {}".format(e))


    def build_hierarchical_model(self):
        try:
            self.logger.info("Hierarchical model")
            self.hierarchical_topics = self.topic_model.hierarchical_topics(self.texts) 
            self.tree=self.topic_model.get_topic_tree(self.hierarchical_topics)
        except Exception as e:
            self.logger.error("Error in Hierarchical model: {}".format(e))

    def save_hierarchical_model(self, file_path):
        try:
            self.logger.info("Saving hierarchical model to file: {}".format(file_path))
            with open(file_path, "w") as text_file:
                text_file.write(self.tree)
        except Exception as e:
            self.logger.error("Error hierarchical model: {}".format(e))

    def compute_topic_similarity(self):
        try:
            self.logger.info("Computing topic similarity")
            self.distance_matrix = cosine_similarity(np.array(self.topic_model.topic_embeddings_)[1:, :])
            self.labels = ["_".join(label.split("_")[1:]) for label in self.topic_model.get_topic_info().Name[1:]]
        except Exception as e:
            self.logger.error("Error computing topic similarity: {}".format(e))

    def save_topic_similarity(self, file_path):
        try:
            self.logger.info("Saving topic similarity to file: {}".format(file_path))
            pd.DataFrame(self.distance_matrix,columns=self.labels, index=self.labels).to_csv(file_path)
        except Exception as e:
            self.logger.error("Error saving topic similarity: {}".format(e))

    def compute_coherence(self):
        def remove_punctuation(text):
            return text.translate(str.maketrans('', '', string.punctuation))
        try:
            self.logger.info("Computing coherence")
            topic_info = self.get_topic_info()
            self.dict =  {'topics': topic_info['Representation']}
            self.cv = Coherence(texts=self.df['text'].apply(remove_punctuation).str.lower().str.split().to_list(),topk=5, measure='c_v')
            self.cohrence = str(self.cv.score(self.dict))
        except Exception as e:
            self.logger.error("Error computing coherence: {}".format(e))

    def compute_diversity(self):
        try:
            self.logger.info("Computing diversity")
            topic_info = self.get_topic_info()
            self.dict =  {'topics': topic_info['Representation']}
            self.dv = TopicDiversity(topk=10)
            self.diversity = str(self.dv.score(self.dict))
        except Exception as e:
            self.logger.error("Error computing diversity: {}".format(e))

    def save_parameters(self, file_path):
        try:
            self.logger.info("Saving parameters to file: {}".format(file_path))
            self.params_df = pd.DataFrame(self.topic_model.get_params().items(), columns=['Parameter', 'Value'])
            self.corpus_info = pd.DataFrame({'Parameter': ['corpus_size'], 'Value': [len(self.df)]})
            self.outlier_prop_row = pd.DataFrame({'Parameter': ['outlier_proportion'], 'Value': [self.topic_model.get_topic_freq(-1)/len(self.df)]})
            self.coherence_row = pd.DataFrame({'Parameter': ['coherence'], 'Value': [self.cohrence]})
            self.diversity_row = pd.DataFrame({'Parameter': ['diversity'], 'Value': [self.diversity]})
            self.params_df = pd.concat([self.outlier_prop_row,self.corpus_info,self.coherence_row,self.diversity_row,self.params_df], ignore_index=True)
            self.params_df.to_excel(file_path, index=False)
        except Exception as e:
            self.logger.error("Error saving parameter table: {}".format(e))

    def create_output_folder(self,topic_model_path):
        self.log_folder = os.path.abspath(os.path.join(topic_model_path, os.pardir))
        self.logger.info(f"Output folder: {self.log_folder}")

    def run_pipeline(self):
        try:
            hierarchical_model_file = os.path.join(self.log_folder, 'topic_tree.txt')
            similarity_matrix_file = os.path.join(self.log_folder, 'similarity_matrix.csv')
            parameter_file = os.path.join(self.log_folder, 'model_parameters.xlsx')

            self.logger.info("Starting pipeline")
            self.load_data()
            self.load_model()
            self.build_hierarchical_model()
            self.save_hierarchical_model(hierarchical_model_file)
            self.compute_topic_similarity()
            self.save_topic_similarity(similarity_matrix_file)
            self.compute_coherence()
            self.compute_diversity()
            self.save_parameters(parameter_file)
            self.logger.info("Pipeline completed successfully.")
        except Exception as e:
            self.logger.error("Pipeline failed with error: {}".format(e))

    def get_document_info(self):
        try:
            self.logger.info("Getting document info")
            return self.topic_model.get_document_info(self.texts)
        except Exception as e:
            self.logger.error("Error getting document info: {}".format(e))

    def get_topic_info(self):
        try:
            self.logger.info("Getting topic info")
            return self.topic_model.get_topic_info()
        except Exception as e:
            self.logger.error("Error getting topic info: {}".format(e))

if __name__ == "__main__":
    # Set up logging
    logging.basicConfig(level=logging.INFO)

    if len(sys.argv) != 3:
        print("Usage: python evaluation_pipeline.py <data_path> <topic_model_path>")
        sys.exit(1)

    data_path = sys.argv[1]
    topic_model_path = sys.argv[2]
    pipeline = EvaluationPipeline(data_path=data_path, topic_model_path=topic_model_path)
    pipeline.run_pipeline()