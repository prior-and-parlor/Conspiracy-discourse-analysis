import pandas as pd 
import numpy as np
#import seaborn as sns
from matplotlib import pyplot as plt


'''loading data'''
template_input = pd.read_csv('./CleanedConspiracyTexts.csv')
template_input.head()

'''renaming column and dropping unnecessary columns'''
template_input.rename(columns = {'Body':0}, inplace = True)
template_input.drop('doc_id', axis=1, inplace=True) #labels=None, axis=0, index=None, columns=None, level=None, inplace=False, errors='raise')


'''removing null and float columns'''
def is_float(value):
    try:
        float(value)
        return True
    except (TypeError, ValueError):
        return False

# Apply the function to the column to identify float values
is_float_mask = template_input[0].apply(is_float)

# Identify null values
is_null_mask = template_input[0].isnull()

# Combine the masks with | operator to get rows that are either float or null
float_or_null_mask = is_float_mask | is_null_mask

# Invert the mask to select rows that are not float and not null
not_float_or_null_mask = ~float_or_null_mask

# Filter the DataFrame to keep rows where 'string_column' is not a float and not null
filtered_df = template_input[not_float_or_null_mask].reset_index(drop=True)

print(len(filtered_df))



# '''step 1 generating all_sent.csv'''

# from emfdscore.scoring import score_docs 

# num_docs = len(filtered_df)

# DICT_TYPE = 'emfd'
# PROB_MAP = 'all'
# SCORE_METHOD = 'bow'
# OUT_METRICS = 'sentiment'
# OUT_CSV_PATH = 'all-sent.csv'

# df = score_docs(filtered_df,DICT_TYPE,PROB_MAP,SCORE_METHOD,OUT_METRICS,num_docs)
# df.to_csv(OUT_CSV_PATH, index=False)




'''step2 generating single_sent.csv'''

# from emfdscore.scoring import score_docs 
# num_docs = len(template_input)
# DICT_TYPE = 'emfd'
# PROB_MAP = 'single'
# SCORE_METHOD = 'bow'
# OUT_METRICS = 'sentiment'
# OUT_CSV_PATH = 'single-sent.csv'
# df = score_docs(template_input,DICT_TYPE,PROB_MAP,SCORE_METHOD,OUT_METRICS,num_docs)
# df.to_csv(OUT_CSV_PATH, index=False)


# '''step3 Generating all-vv.csv'''

# from emfdscore.scoring import score_docs 

# num_docs = len(filtered_df)

# DICT_TYPE = 'emfd'
# PROB_MAP = 'all'
# SCORE_METHOD = 'bow'
# OUT_METRICS = 'vice-virtue'
# OUT_CSV_PATH = 'all-vv.csv'

# df = score_docs(filtered_df,DICT_TYPE,PROB_MAP,SCORE_METHOD,OUT_METRICS,num_docs)
# df.to_csv(OUT_CSV_PATH, index=False)



# '''step4 Generating single-vv.csv'''
# from emfdscore.scoring import score_docs 

# num_docs = len(filtered_df)

# DICT_TYPE = 'emfd'
# PROB_MAP = 'single'
# SCORE_METHOD = 'bow'
# OUT_METRICS = 'vice-virtue'
# OUT_CSV_PATH = 'single-vv.csv'

# df = score_docs(filtered_df,DICT_TYPE,PROB_MAP,SCORE_METHOD,OUT_METRICS,num_docs)
# df.to_csv(OUT_CSV_PATH, index=False)






from concurrent.futures import ProcessPoolExecutor
from emfdscore.scoring import score_docs
import pandas as pd
import concurrent

# Define a function that encapsulates the steps you want to run in parallel
def run_step(filtered_df_or_input, dict_type, prob_map, score_method, out_metrics, out_csv_path):
    num_docs = len(filtered_df_or_input)
    df = score_docs(filtered_df_or_input, dict_type, prob_map, score_method, out_metrics, num_docs)
    df.to_csv(out_csv_path, index=False)
    print(f"{out_csv_path} has been created.")

# Define your parameters for each step
step1_params = {
    'filtered_df_or_input': filtered_df,
    'dict_type': 'emfd',
    'prob_map': 'all',
    'score_method': 'bow',
    'out_metrics': 'sentiment',
    'out_csv_path': 'all-sent.csv',
}

step2_params = {
    'filtered_df_or_input': filtered_df,
    'dict_type': 'emfd',
    'prob_map': 'single',
    'score_method': 'bow',
    'out_metrics': 'sentiment',
    'out_csv_path': 'single-sent.csv',
}

step3_params = {
    'filtered_df_or_input': filtered_df,
    'dict_type': 'emfd',
    'prob_map': 'single',
    'score_method': 'bow',
    'out_metrics': 'vice-virtue',
    'out_csv_path': 'single-vv.csv',
}

step4_params = {
    'filtered_df_or_input': filtered_df,
    'dict_type': 'emfd',
    'prob_map': 'a;;',
    'score_method': 'bow',
    'out_metrics': 'vice-virtue',
    'out_csv_path': 'all-vv.csv',
}



if __name__ == '__main__':
    # Use ProcessPoolExecutor to run the steps in parallel
    with ProcessPoolExecutor() as executor:
        # Start the operations and mark each future with its step
        future_to_step = {
            executor.submit(run_step, **step1_params): 'step1',
            executor.submit(run_step, **step2_params): 'step2',
            executor.submit(run_step, **step3_params): 'step3',
            executor.submit(run_step, **step4_params): 'step4',
        }
        for future in concurrent.futures.as_completed(future_to_step):
            step = future_to_step[future]
            try:
                data = future.result()
            except Exception as exc:
                print(f"{step} generated an exception: {exc}")
            else:
                print(f"{step} completed successfully.")
