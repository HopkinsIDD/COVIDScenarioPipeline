import setuptools

# that's to make steps exist in the directory, though it needs the right numba version
# right away
from SEIR.steps_source import cc as steps_cc
from SEIR.dev.steps import cc as steps_ode


with open("requirements.txt") as f:
    requirements = f.read().splitlines()

setuptools.setup(
    name="SEIR",
    version="0.2",
    author="Joseph Lemaitre",
    author_email="jo.lemaitresamra@gmail.com",
    description="SEIR simulation",
    packages=setuptools.find_packages(exclude=["*.test*"]),
    ext_modules=[
        steps_cc.distutils_extension()
    ],  # , steps_ode.distutils_extension()], # LazyExtensions(),#
    license="LICENSE",
    install_requires=requirements,
    # setup_requires=['numba==0.53.1'],
    python_requires=">=3.7",
    zip_safe=True,
    scripts=["SEIR/simulate.py"],
)

# https://stackoverflow.com/questions/58514490/how-to-use-numba-extension-in-setup-py
steps_cc.compile()


setuptools.setup(
    name="Outcomes",
    version="0.1",
    author="Joseph Lemaitre",
    author_email="jo.lemaitresamra@gmail.com",
    description="Health Outcome simulation",
    packages=setuptools.find_packages(exclude=["*.test*"]),
    license="LICENSE",
    install_requires=requirements,
    python_requires=">=3.7",
    zip_safe=True,
    scripts=["Outcomes/simulate.py"],
)
