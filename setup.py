import setuptools

from SEIR.steps_source import cc as steps_cc

with open("requirements.txt") as f:
    requirements = f.read().splitlines()

setuptools.setup(
    name="SEIR",
    version="0.1",
    author="Joshua Kaminsky",
    author_email="jkaminsky@jhu.edu",
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
