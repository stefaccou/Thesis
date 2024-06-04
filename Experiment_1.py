import torch
from transformers import AutoTokenizer, AutoModel
import pandas as pd
import os
import re
import plotly.express as px
from sklearn.decomposition import PCA
from sklearn.mixture import GaussianMixture
import numpy as np
import warnings
#!pip install sacremoses
#!unzip Corpus.zip

def run(BERT_model):
    if BERT_model.lower() == "flaubert":
        tokenizer = AutoTokenizer.from_pretrained("flaubert/flaubert_base_cased")
        model = AutoModel.from_pretrained("flaubert/flaubert_base_cased")
    elif BERT_model.lower() == "camembert":
        tokenizer = AutoTokenizer.from_pretrained("camembert-base")
        model = AutoModel.from_pretrained("camembert-base")
    else:
        raise ValueError("Model not supported")

    cuda = torch.device('cuda')
    model.cuda()
    WSD_PATTERN = r' (\w+)/\w+\.\w\.\d+'  # (Woord), letterlijke slash, woord, letterlijke punt, letter, cijfer(s)


    def get_target_word(sentence, target_word):
        word_match = re.search(r'\b{}\w*\b'.format(target_word), sentence, re.IGNORECASE)
        if word_match is None:
            return None
        return word_match.group(0)


    def preproc_sentence(sentence):
        sent_preproc = re.sub(WSD_PATTERN, r'\1', sentence)  # Alleen woord blijft over
        return sent_preproc


    def find_position_word(sentence, word):
        ids_word = tokenizer.encode(word)
        tokens_word = tokenizer.convert_ids_to_tokens(ids_word)[1:-1]
        ids_sentence = tokenizer.encode(sentence)
        tokens_sentence = tokenizer.convert_ids_to_tokens(ids_sentence)
        #print(tokens_sentence)  # if the code breaks, enabling this helps to see if tokenization of the sentence is what breaks it
        if len(tokens_word) == 1:
            position_word_in_sentence = tokens_sentence.index(tokens_word[0])
        else:
            position_word_in_sentence = [tokens_sentence.index(tokens_word[0]), tokens_sentence.index(tokens_word[-1]) + 1]
        return position_word_in_sentence


    @torch.no_grad()
    def encode_sentence_and_extract_position(sentence, position):
        ids = tokenizer.encode(sentence)
        bert_output = model.forward(torch.tensor(ids, device=cuda).unsqueeze(0))
        final_layer_embeddings = bert_output['last_hidden_state'].squeeze()
        if type(position) == int:
            return final_layer_embeddings[position].unsqueeze(0)
        elif type(position) == list:
            return torch.mean(
                final_layer_embeddings[position[0]:position[1]], 0
            ).unsqueeze(0)

    def get_embeddings_from_dataframe(dataframe):
        word = dataframe['source'].iloc[0].lower()
        embeddings = []
        rows_to_keep = []
        for index, sentence in dataframe.iterrows():
            matched_word_form = get_target_word(sentence['match'], sentence['source'])
            # print(sentence["match"], matched_word_form, "\n")
            if matched_word_form == word:

                prep_sent = preproc_sentence(sentence['match'])
                try:
                    position = find_position_word(prep_sent, matched_word_form)
                    embeddings.append(encode_sentence_and_extract_position(prep_sent, position))
                    rows_to_keep.append(sentence)
                except:
                    print(f"Error with sentence: {sentence['match']}")
        new_df = pd.DataFrame(rows_to_keep)
        return new_df, embeddings

    # functie om dataframe uit te breiden met drie PCA-waarden
    def extend_df_with_pca(df, m_np):
        df_new = df.copy()

        pca = PCA(n_components=3)
        components = pca.fit_transform(m_np)

        df_new.insert(1, 'x', components[:, 0])
        df_new.insert(2, 'y', components[:, 1])
        df_new.insert(3, 'z', components[:, 2])

        return df_new


    Word_list = ["Avocat", "Bien", "Bureau", "Faculté", "Filer", "Glace", "Souris", "Supporter", "Tirer", "Tour", "Vol"]
    BERT_Model = BERT_model.capitalize()

    length = len(Word_list)
    print(f" {BERT_Model}, with the words: {Word_list}\n", "-"*25, "\n")

    for Word in Word_list:
        print(f"\n\nRunning {Word} with {BERT_Model}. Word {Word_list.index(Word) + 1} out of {length}")
        df = pd.read_csv(f'Corpus/Final/Manual/{Word}.csv', sep=";", encoding="utf-8", header=0)

        if not os.path.exists(f"{BERT_Model}/Experiment_1/Visuals/{Word}"):
            os.makedirs(f"{BERT_Model}/Experiment_1/Visuals/{Word}")
        if not os.path.exists(f"{BERT_Model}/Experiment_1/Curated"):
            os.makedirs(f"{BERT_Model}/Experiment_1/Curated")

        new_df, embeddings = get_embeddings_from_dataframe(df)
        emb_matrix = torch.cat(embeddings, dim=0)
        print(f"{len(embeddings)} left of {len(df)} original sentences")
        # constructie van numpy-matrix; die matrix kunnen we gebruiken voor PCA
        matrix_np = emb_matrix.cpu().detach().numpy()

        df_pca_og = extend_df_with_pca(new_df, matrix_np)

        # We tellen het aantal voorkomens van iedere sense
        sense_counts = df_pca_og["sense"].value_counts()
        # visualisatie in 2D; kleuren voor verschillende klassen;
        # hoveren over data toont zinnen

        fig_2d = px.scatter(df_pca_og, x='x', y='y', color='sense',
                            hover_data='match',
                            template="plotly_white")
        fig_2d.update_layout(
            title={
                'text': f"Manual annotation: {Word}"
                        f"<br><sub>Different senses: {[(sense, count) for sense, count in sense_counts.items()]}</sub>",
                'y': 0.9,
                'x': 0.5,
                'xanchor': 'center',
                'yanchor': 'top'},
            legend_title="Senses"
        )
        #fig_2d.show()
        # visualisatie in 3D
        fig_3d = px.scatter_3d(
            df_pca_og, x='x', y='y', z='z', color='sense',
            hover_data='match',
            template="plotly_white",
        )
        fig_3d.update_layout(
            title={
                'text': f"Manual annotation: {Word}"
                        f"<br><sub>Different senses: {[(sense, count) for sense, count in sense_counts.items()]}</sub>",
                'y': 0.9,
                'x': 0.5,
                'xanchor': 'center',
                'yanchor': 'top'},
            legend_title="Senses"
        )
        #fig_3d.show()
        warnings.filterwarnings("ignore")

        df_pca_numerical_og = df_pca_og.select_dtypes(include=[np.number])
        # Assuming data_og is your data
        data_og = df_pca_numerical_og.to_numpy()

        # List to hold BIC values
        bic_values = []

        # Range of potential cluster numbers to test
        cluster_range = range(1, 11)

        # Fit Gaussian Mixture Models for each number of clusters
        for i in cluster_range:
            #print(f"Fitting model with {i} clusters")
            gmm = GaussianMixture(n_components=i, random_state=0).fit(data_og)
            bic_values.append(gmm.bic(data_og))

        # Find the number of clusters that gives the minimum BIC
        optimal_clusters = cluster_range[np.argmin(bic_values)]
        print(f"Optimal number of clusters: {optimal_clusters}")

        # Fit the optimal model
        gmm_optimal = GaussianMixture(n_components=optimal_clusters).fit(data_og)

        # Predict the cluster for each data point
        clusters_og = gmm_optimal.predict(data_og)
        # We want them to start counting at "1" instead of "0"
        clusters_og += 1

        # Add the clusters to the dataframe
        df_pca_og["cluster"] = clusters_og

        # Calculating the clustering score

        # We calculate a score for the clustering (starting before the outliers are removed)
        # Group the dataframe by "sense" and "cluster", and calculate the size of each group
        df_grouped = df_pca_og.groupby(["sense", "cluster"]).size().reset_index(name="count")

        # Sort these clusters by size in descending order
        df_grouped = df_grouped.sort_values(by="count", ascending=False)

        # Initialize an empty dictionary to store the cluster numbers that have been assigned as default clusters
        # If the cluster number is not taken, we assign it to the corresponding "sense"
        # Else, we try to assign it to the next cluster number
        cluster_dict = {}
        for index, row in df_grouped.iterrows():
            if row["sense"] not in cluster_dict:
                if row["cluster"] not in cluster_dict.values():
                    cluster_dict[row["sense"]] = row["cluster"]

        # We add "sense" values that have no entry in cluster_dict and set value to 0 (always seen as wrong)
        for sense in df_pca_og["sense"].unique():
            if sense not in cluster_dict:
                cluster_dict[sense] = 0

        # Add a new column "default" to the original dataframe
        df_pca_og["default"] = df_pca_og.apply(lambda x: x["cluster"] == cluster_dict[x["sense"]], axis=1)
        # We calculate the percentage of default clusters
        percentage_default = (df_pca_og["default"].sum() / len(df_pca_og)) * 100
        # We also calculate this separately for each "sense"
        percentage_default_mean = df_pca_og.groupby("sense")["default"].mean() * 100

        # We want the mean score across all senses, as it does not mean a lot if a program can correctly define one big cluster containing most of the data and fail at all other senses.
        percentage_weighted = percentage_default_mean.mean()
        print("Score for each", percentage_default_mean)
        print("Overall score", percentage_default)
        # We delete all clusters that consist of one or two points from the visualisation.
        # We do this by checking if the cluster is in the list of unique clusters that only occur once or twice
        clusters_to_delete = [cluster for cluster in np.unique(clusters_og) if len(clusters_og[clusters_og == cluster]) <= 2]
        # We delete the clusters from the original dataframe
        df_pca = df_pca_og[~df_pca_og["cluster"].isin(clusters_to_delete)]
        # We delete the clusters from the clusters
        before_removing = len(np.unique(clusters_og))
        clusters = clusters_og[~np.isin(clusters_og, clusters_to_delete)]
        points_removed = len(df_pca_og) - len(df_pca)
        print(f"Removed {len(np.unique(clusters_og)) - len(np.unique(clusters))} clusters, which removed {points_removed} points")
        fig_2d_cluster = px.scatter(df_pca, x="x", y="y", color="sense", symbol=clusters,
                         hover_data='match',
                         template="plotly_white")
        fig_2d_cluster.update_layout(
            title={
                'text': f"Clusters {Word}<br><sub>Clusters: {len(np.unique(clusters))}{(', Outliers removed' if points_removed else '')}</sub>"
                ,  #f"<sub><br>Clustering score: {percentage_weighted:.2f}%</sub>",
                'y':0.9,
                'x':0.5,
                'xanchor': 'center',
                'yanchor': 'top'})
        #fig_2d_cluster.show()
        fig_3d_cluster = px.scatter_3d(df_pca, x="x", y="y", z="z", color="sense", symbol=clusters,
                         hover_data='match',
                         template="plotly_white")
        fig_3d_cluster.update_layout(
            title={
                'text': f"Clusters {Word}<br><sub>Clusters: {len(np.unique(clusters))}{(', Outliers removed' if points_removed else '')}</sub>"
                ,  #f"<sub><br>Clustering score: {percentage_weighted:.2f}%</sub>",
                'y':0.9,
                'x':0.5,
                'xanchor': 'center',
                'yanchor': 'top'})
        #fig_3d_cluster.show()



        #----------------------------------------------------------
        # EXPERIMENT 2
        print("\t Running experiment with automatic download")
        #----------------------------------------------------------

        try:
            df_automatic = pd.read_csv(f'Corpus/Final/Automatic/{Word}.csv', sep=";", encoding="utf-8", header=0)
            # We add the original df to df_automatic
            df_automatic = pd.concat([df, df_automatic])
            new_df_automatic, embeddings_automatic = get_embeddings_from_dataframe(df_automatic)
            emb_matrix_automatic = torch.cat(embeddings_automatic, dim=0)
            print(f"{len(embeddings_automatic)} left of {len(df_automatic)} sentences")
            df_pca_automatic = extend_df_with_pca(new_df_automatic, emb_matrix_automatic.cpu().detach().numpy())
            fig_automatic = px.scatter(df_pca_automatic, x='x', y='y',
                            color="sense",
                            hover_data='match',
                             template="plotly_white",
                             title = f"Random download: {Word}")
            #fig_automatic.show()
            warnings.filterwarnings("ignore")

            df_pca_numerical_automatic = df_pca_automatic.select_dtypes(include=[np.number])
            data_automatic = df_pca_numerical_automatic.to_numpy()
            # List to hold BIC values
            bic_values_automatic = []

            # Range of potential cluster numbers to test
            cluster_range = range(1, 11)

            # Fit Gaussian Mixture Models for each number of clusters
            for i in cluster_range:
                # print(f"Fitting model with {i} clusters")
                gmm = GaussianMixture(n_components=i, random_state=0).fit(data_automatic)
                bic_values_automatic.append(gmm.bic(data_automatic))

            # Find the number of clusters that gives the minimum BIC
            optimal_clusters_automatic = cluster_range[np.argmin(bic_values_automatic)]
            print(f"Optimal number of clusters: {optimal_clusters_automatic}")

            # Fit the optimal model
            gmm_optimal_automatic = GaussianMixture(n_components=optimal_clusters_automatic).fit(data_automatic)

            # Predict the cluster for each data point
            clusters_automatic = gmm_optimal_automatic.predict(data_automatic)
            # We want them to start counting at "1" instead of "0"
            clusters_automatic += 1

            # Add the clusters to the dataframe
            df_pca_automatic["cluster"] = clusters_automatic
            # We delete occasional clusters that consist of one or two points to visualise data more clearly.
            # We do this by checking if the cluster is in the list of unique clusters that only occur once or twice
            clusters_to_delete = [cluster for cluster in np.unique(clusters_automatic) if
                                  len(clusters_automatic[clusters_automatic == cluster]) <= 2]
            # We delete the clusters from the original dataframe
            df_pca_automatic = df_pca_automatic[~df_pca_automatic["cluster"].isin(clusters_to_delete)]
            # We delete these clusters from the clusters
            before_removing = len(np.unique(clusters_automatic))
            clusters_automatic = clusters_automatic[~np.isin(clusters_automatic, clusters_to_delete)]
            print(f"Removed {before_removing - len(np.unique(clusters_automatic))} clusters")


            fig_2d_cluster_automatic = px.scatter(df_pca_automatic, x="x", y="y",
                                                  color=clusters_automatic,
                                                  hover_data='match',
                                                  symbol="sense",
                                                  color_discrete_sequence=["lightgrey"],
                                                  template="plotly_white",
                                                  title=f"Clusters {Word}, with added random webcrawl sample")
            fig_2d_cluster_automatic.update(layout_coloraxis_showscale=False)
            # fig_2d_cluster_automatic.show()
            fig_3d_cluster_automatic = px.scatter_3d(df_pca_automatic, x="x", y="y", z="z",
                                                     color=clusters_automatic,
                                                     hover_data='match',
                                                     symbol="sense",
                                                     color_discrete_sequence=["lightgrey"],
                                                     template="plotly_white",
                                                     title=f"Clusters {Word}, with added random webcrawl sample")
            fig_3d_cluster_automatic.update(layout_coloraxis_showscale=False)

            # fig_3d_cluster_automatic.show()
            # Clusters of automatical downloads
            fig_2d_cluster_automatic.write_html(f"{BERT_Model}/Experiment_1/Visuals/{Word}/{Word}_Experiment_2d.html")
            fig_3d_cluster_automatic.write_html(f"{BERT_Model}/Experiment_1/Visuals/{Word}/{Word}_Experiment_3d.html")

            # We also calculate a clustering score (of the annotated senses) in this big dataset
            df_grouped_automatic = df_pca_automatic.groupby(["sense", "cluster"]).size().reset_index(name="count")
            df_grouped_automatic = df_grouped_automatic[df_grouped_automatic["sense"] != "???"]
            df_grouped_automatic = df_grouped_automatic.sort_values(by="count", ascending=False)

            cluster_dict_automatic = {}
            for index, row in df_grouped_automatic.iterrows():
                if row["sense"] not in cluster_dict_automatic:
                    if row["cluster"] not in cluster_dict_automatic.values():
                        cluster_dict_automatic[row["sense"]] = row["cluster"]

            # We add "sense" values that have no entry in cluster_dict and set value to 0 (always seen as wrong)
            for sense in df_grouped_automatic["sense"].unique():
                if sense not in cluster_dict_automatic:
                    cluster_dict_automatic[sense] = 0
            df_pca_semi_automatic = df_pca_automatic[df_pca_automatic["sense"] != "???"]
            df_pca_semi_automatic["default"] = df_pca_semi_automatic.apply(lambda x: x["cluster"] == cluster_dict_automatic[x["sense"]], axis=1)
            percentage_default = (df_pca_semi_automatic["default"].sum() / len(df_pca_semi_automatic)) * 100
            percentage_default_mean = df_pca_semi_automatic.groupby("sense")["default"].mean() * 100
            percentage_weighted = percentage_default_mean.mean()
            print("Score for each", percentage_default_mean)
            print("Overall score", percentage_default)
            if not os.path.exists(f"{BERT_Model}/Experiment_1/Automatic"):
                os.makedirs(f"{BERT_Model}/Experiment_1/Automatic")
            df_pca_semi_automatic.to_csv(f'{BERT_Model}/Experiment_1/Automatic/{Word}.csv', sep=";", encoding="utf-8", index=False)
        except:
            print("No automatic download available")


        # We save the plots in the directory

        # Original plots with all embeddings
        fig_2d.write_html(f"{BERT_Model}/Experiment_1/Visuals/{Word}/{Word}_2d.html")
        fig_3d.write_html(f"{BERT_Model}/Experiment_1/Visuals/{Word}/{Word}_3d.html")

        # Clusters after removing outliers
        fig_2d_cluster.write_html(f"{BERT_Model}/Experiment_1/Visuals/{Word}/{Word}_Clusters_2d.html")
        fig_3d_cluster.write_html(f"{BERT_Model}/Experiment_1/Visuals/{Word}/{Word}_Clusters_3d.html")

        # We also save df_pca_og with added clusters to a csv file
        df_pca_og.to_csv(f'{BERT_Model}/Experiment_1/Curated/{Word}.csv', sep=";", encoding="utf-8", index=False)

