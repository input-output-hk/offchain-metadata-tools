from locust import HttpUser, TaskSet, constant, task

from generator.subjects import get_random_subject, get_invalid_subject
from generator.properties import get_random_property, get_invalid_property

class QueryUser(HttpUser):
    host = "https://metadata.cardano-testnet.iohkdev.io"

    # between each task a user will wait 1 second
    wait_time = constant(1)

    # task(n) assigns the probability of that task being performed by the user. 
    # in this code, task(1) is equal to 10% and task(2) is equal to 20%

    @task(2)
    def get_metadata_subject(self):
        subject = get_random_subject()
        self.client.get(f"/metadata/{subject}")

    @task(2)
    def get_metadata_subject_properties(self):
        subject = get_random_subject()

        self.client.get(f"/metadata/{subject}/properties")

    # @task(2)
    # def get_metadata_subject_properties_property(self):
    #     subject = get_random_subject()
    #     property = get_random_property()

    #     self.client.get(f"/metadata/{subject}/properties/{property}")

    # @task(1)
    # def get_metadata_query(self):
    #     self.client.get(f"/metadata/query")

    @task(1)
    def get_metadata_subject_invalid(self):
        subject = get_invalid_subject()

        with self.client.get(f"/metadata/{subject}", catch_response=True) as response:
            if response.status_code == 404:
                response.success()

    @task(1)
    def get_metadata_subject_properties_invalid(self):
        subject = get_invalid_subject()

        with self.client.get(f"/metadata/{subject}/properties", catch_response=True) as response:
            if response.status_code == 404:
                response.success()

    @task(1)
    def get_metadata_subject_properties_property_invalid(self):
        subject = get_invalid_subject()
        property = get_invalid_property()

        with self.client.get(f"/metadata/{subject}/properties/{property}", catch_response=True) as response:
            if response.status_code == 404:
                response.success()