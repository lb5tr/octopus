import selenium, unittest, time
from selenium import webdriver

test_suite_username = 'test-user'
test_suite_password = 'abc'

class FunctionalTests(unittest.TestCase):
    def setUp(self):
        self.driver = webdriver.Chrome()

    def go_there(self):
        self.driver.get("http://octopus.lan/octopus/simple.html")

    def login(self):
        loginForm = self.driver.find_element_by_id("userName")
        passwordForm = self.driver.find_element_by_id("password")
        button = self.driver.find_element_by_id("login")

        loginForm.send_keys(test_suite_username)
        passwordForm.send_keys(test_suite_password)
        button.click()
        time.sleep(3)


    def test_simple_login(self):
        self.go_there()
        self.login()

        loggedZone = self.driver.find_element_by_id("loggedZone")

        self.assertTrue(loggedZone.is_displayed())
        self.driver.close()

    def get_by_id(self, elemid):
        return self.driver.find_element_by_id(elemid)

    def test_create_channel(self):
        self.go_there()
        self.login()

        name = self.get_by_id('name')
        capacity = self.get_by_id('capacity')
        password = self.get_by_id('cpassword')
        button = self.get_by_id('createChannel')

        name.send_keys('test-channel')
        time.sleep(1)
        capacity.send_keys('1337')
        time.sleep(1)
        password.send_keys('password')
        time.sleep(1)
        button.click()

        chlist = self.get_by_id('channelsList')

        self.assertTrue('test-channel' in chlist.text)
        self.driver.close()

if __name__ == '__main__':
    suite = unittest.TestLoader().loadTestsFromTestCase(FunctionalTests)
    unittest.TextTestRunner(verbosity=2).run(suite)
