

import csv as csv
import pandas

import numpy as np


# First, read in test.csv
test_file = open('test.csv', 'rb')
test_file_object = csv.reader(test_file)
header = test_file_object.next()

# Also open the a new file so I can write to it. Call it something descriptive
# Finally, loop through each row in the train file, and look in column index [3] (which is 'Sex')
# Write out the PassengerId, and my prediction.

predictions_file = open("secondTry.csv", "wb")
predictions_file_object = csv.writer(predictions_file)
predictions_file_object.writerow(["PassengerId", "Survived"])	# write the column headers
df = pandas.read_csv('test.csv')
for passenger_index, passenger in df.iterrows():

        if passenger['Sex']=='female' or passenger['Pclass']<=2 and passenger['Age']<18:

            predictions_file_object.writerow([passenger[0], "1"])
        else:
            predictions_file_object.writerow([passenger[0], "0"])
test_file.close()												# Close out the files.
predictions_file.close()
