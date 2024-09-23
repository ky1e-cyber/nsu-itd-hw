#!/bin/sh

python3 -m venv ./venv &&
  source ./venv/bin/activate && 
    wget https://github.com/cdm-processors/cdm-devkit/releases/download/0.2.1/cdm_devkit-0.2.1-py3-none-any.whl &&
      pip3 install cdm_devkit-0.2.1-py3-none-any.whl   
    
