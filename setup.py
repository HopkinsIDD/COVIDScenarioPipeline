import setuptools

from SEIR.steps_source import cc as steps_cc

steps_cc.compile()

with open("requirements.txt") as f:
    requirements = f.read().splitlines()

setuptools.setup(
    name="SEIR",
    version="0.2",
    author="Joseph Lemaitre",
    author_email="jo.lemaitresamra@gmail.com",
    description="SEIR simulation",
    packages=setuptools.find_packages(exclude=["*.test*"]),
    ext_modules=[steps_cc.distutils_extension()],
    license="LICENSE",
    install_requires=requirements,
    python_requires='>=3.7',
    zip_safe=True,
    scripts=[
        "SEIR/simulate.py"
    ],
)

setuptools.setup(
    name="Outcomes",
    version="0.1",
    author="Joseph Lemaitre",
    author_email="jo.lemaitresamra@gmail.com",
    description="Health Outcome simulation",
    packages=setuptools.find_packages(exclude=["*.test*"]),
    license="LICENSE",
    install_requires=requirements,
    python_requires='>=3.7',
    zip_safe=True,
    scripts=[
        "Outcomes/simulate.py"
    ],
)
