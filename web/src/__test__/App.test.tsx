import React from "react";
import { render, fireEvent, screen, act } from "@testing-library/react";

import { BrowserRouter } from "react-router-dom";
import App from "app/App";

describe("should follow basic workflow without errors", () => {
  it("should go up and down synthesis tree correctly", async () => {
    await act(async () => {
      render(
        <BrowserRouter>
          <App />
        </BrowserRouter>
      );

      // let subforestScreenNavButton = await screen.findByText("Subforest");
      // await act(async () => {
      //   fireEvent.click(subforestScreenNavButton);
      // });

      // // when should it be stopped?
      // for (let i = 0; i < 20; i++) {
      //   let forwardButton = await screen.findByText("Forward");

      //   await act(async () => {
      //     fireEvent.click(forwardButton);
      //   });
      // }

      // let testbenchScreenNavButton = await screen.findByText("Testbench");
      // await act(async () => {
      //   fireEvent.click(testbenchScreenNavButton);
      // });

      // subforestScreenNavButton = await screen.findByText("Subforest");
      // await act(async () => {
      //   fireEvent.click(subforestScreenNavButton);
      // });

      // // when should it be stopped?
      // for (let i = 0; i < 20; i++) {
      //   let backwordButton = await screen.findByText("Back");

      //   await act(async () => {
      //     fireEvent.click(backwordButton);
      //   });
      // }
    });
  });

  // it("should switch between tabs correctly", async () => {
  //   await act(async () => {
  //     render(
  //       <BrowserRouter>
  //         <App />
  //       </BrowserRouter>
  //     );

  //     let synthesisStepDropdown = await screen.findByText("Synthesis step");
  //     await act(async () => {
  //       fireEvent.click(synthesisStepDropdown);
  //     });

  //     let bestThreadDropdownOption = await screen.findByText("best-thread");
  //     await act(async () => {
  //       fireEvent.click(bestThreadDropdownOption);
  //     });

  //     let subforestButton = await screen.findByText("Subforest");
  //     await act(async () => {
  //       fireEvent.click(subforestButton);
  //     });

  //     let processButton = await screen.findByText("Process");
  //     await act(async () => {
  //       fireEvent.click(processButton);
  //     });

  //     let testbenchButton = await screen.findByText("Testbench");
  //     await act(async () => {
  //       fireEvent.click(testbenchButton);
  //     });

  //     let debugButton = await screen.findByText("Debug");
  //     await act(async () => {
  //       fireEvent.click(debugButton);
  //     });
  //   });
  // });
});
