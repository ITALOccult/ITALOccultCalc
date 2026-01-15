using System;
using System.Collections.Generic;

namespace AOTA
{
	internal class Chi2
	{
		internal static double[] RawSignal;

		internal static double[] RawSignal_SD;

		internal static double[] SignalForComparison;

		internal static bool[] ValidForComparison;

		internal static double[] Test;

		internal static double[] TestForComparison;

		internal static double[] NoiseForTest;

		internal static int LengthOfSignal = 0;

		internal static double[,,] TransitionSignals;

		internal static float[] OffsetOfStartFromMiddle;

		internal static int MonteCarloTrials = 200;

		internal static Chi2_MinimaPositions Chi2Entry;

		internal static List<Chi2_MinimaPositions> Chi2_D = new List<Chi2_MinimaPositions>();

		internal static List<Chi2_MinimaPositions> Chi2_R = new List<Chi2_MinimaPositions>();

		internal static double[] Chi2_D_Minimums = new double[AOTAData.MaximimumTransitions + 1];

		internal static double[] Chi2_R_Minimums = new double[AOTAData.MaximimumTransitions + 1];

		internal static int[] Chi2_D_MinimumPosns = new int[AOTAData.MaximimumTransitions + 1];

		internal static int[] Chi2_R_MinimumPosns = new int[AOTAData.MaximimumTransitions + 1];

		internal static int[] Chi2_D_CMcounts = new int[21];

		internal static int[] Chi2_R_CMcounts = new int[21];

		internal static void InitialiseVariables()
		{
			Chi2_D_Minimums = new double[AOTAData.MaximimumTransitions + 1];
			Chi2_R_Minimums = new double[AOTAData.MaximimumTransitions + 1];
			Chi2_D_MinimumPosns = new int[AOTAData.MaximimumTransitions + 1];
			Chi2_R_MinimumPosns = new int[AOTAData.MaximimumTransitions + 1];
			Chi2_D_CMcounts = new int[21];
			Chi2_R_CMcounts = new int[21];
		}

		internal static void CreateTransitionSignals()
		{
			TransitionSignals = new double[AOTAData.MaximimumTransitions + 3, AOTAData.MaximimumTransitions + 3, 2];
			OffsetOfStartFromMiddle = new float[AOTAData.MaximimumTransitions + 3];
			for (int i = 0; i <= AOTAData.MaximimumTransitions; i++)
			{
				double num = (double)i / 2.0;
				double num2 = num + 0.50000000001;
				OffsetOfStartFromMiddle[i] = (int)Math.Floor(num2);
				for (int j = 0; j <= i; j++)
				{
					double num3 = (double)j - num2;
					double num4;
					if (num3 / num <= -1.0)
					{
						num4 = 1.0;
					}
					else if (num3 / num >= 1.0)
					{
						num4 = 0.0;
					}
					else
					{
						double num5 = 2.0 * Math.Acos(num3 / num);
						num4 = (num5 - Math.Sin(num5)) / 2.0 / Math.PI;
					}
					TransitionSignals[j, i, 0] = num4;
					TransitionSignals[j, i, 1] = 1.0 - num4;
				}
			}
		}

		internal static void CreateTestSignalArray(int SlopeValue, bool IsDisappear, int OffsetFromStartOfMeasurementRegion)
		{
			int num = 0;
			int num2 = 0;
			double Mean;
			double SD;
			double Variance;
			double Mean2;
			int num3;
			int num4;
			double num5;
			if (IsDisappear)
			{
				num3 = AOTAData.X1 - AOTAData.X0;
				_ = AOTAData.X2;
				_ = AOTAData.X0;
				num4 = AOTAData.X3 - AOTAData.X0;
				if (AOTAData.X3 < AOTAData.X2)
				{
					AOTAData.X3 = AOTAData.X2;
					num4 = AOTAData.X3 - AOTAData.X0;
				}
				AOTAData.GetMean_SD(AOTAData.X0, AOTAData.X1, out Mean, out SD, out Variance);
				AOTAData.GetMean_SD(AOTAData.X2, AOTAData.X3, out Mean2, out SD, out Variance);
				num5 = Mean - Mean2;
				LengthOfSignal = num4 + 1;
				Test = new double[LengthOfSignal];
				SignalForComparison = new double[LengthOfSignal];
				ValidForComparison = new bool[LengthOfSignal];
				TestForComparison = new double[LengthOfSignal];
				RawSignal = new double[LengthOfSignal];
				RawSignal_SD = new double[LengthOfSignal];
				num = num3 - (int)OffsetOfStartFromMiddle[SlopeValue] + OffsetFromStartOfMeasurementRegion;
				num2 = num + SlopeValue + 1;
				for (int i = 0; i < num; i++)
				{
					Test[i] = Mean;
				}
				for (int j = 0; j < SlopeValue + 2; j++)
				{
					int num6 = num + j;
					if (num6 >= 0)
					{
						if (num6 > num4)
						{
							break;
						}
						Test[num6] = Mean2 + num5 * TransitionSignals[j, SlopeValue, 0];
					}
				}
				for (int k = num2; k <= num4; k++)
				{
					Test[k] = Mean2;
				}
				return;
			}
			num3 = AOTAData.X5 - AOTAData.X4;
			_ = AOTAData.X6;
			_ = AOTAData.X4;
			num4 = AOTAData.X7 - AOTAData.X4;
			if (num3 < 0)
			{
				AOTAData.X4 = AOTAData.X5;
				num3 = AOTAData.X5 - AOTAData.X4;
				_ = AOTAData.X6;
				_ = AOTAData.X4;
				num4 = AOTAData.X7 - AOTAData.X4;
			}
			AOTAData.GetMean_SD(AOTAData.X6, AOTAData.X7, out Mean, out SD, out Variance);
			AOTAData.GetMean_SD(AOTAData.X4, AOTAData.X5, out Mean2, out SD, out Variance);
			num5 = Mean - Mean2;
			LengthOfSignal = num4 + 1;
			Test = new double[LengthOfSignal];
			TestForComparison = new double[LengthOfSignal];
			SignalForComparison = new double[LengthOfSignal];
			ValidForComparison = new bool[LengthOfSignal];
			RawSignal = new double[LengthOfSignal];
			RawSignal_SD = new double[LengthOfSignal];
			num = num3 - (int)OffsetOfStartFromMiddle[SlopeValue] + OffsetFromStartOfMeasurementRegion;
			num2 = num + SlopeValue + 1;
			for (int l = 0; l < num; l++)
			{
				Test[l] = Mean2;
			}
			for (int m = 0; m <= SlopeValue + 2; m++)
			{
				int num6 = num + m;
				if (num6 >= 0)
				{
					if (num6 > num4)
					{
						break;
					}
					Test[num6] = Mean2 + num5 * TransitionSignals[m, SlopeValue, 1];
				}
			}
			for (int n = num2; n <= num4; n++)
			{
				Test[n] = Mean;
			}
		}

		internal static void TransferObservedDataToAnalyse(bool Disappear)
		{
			double num;
			double num2;
			if (Disappear)
			{
				num = (AOTAData.VarianceBefore - AOTAData.VarianceDuring) / (AOTAData.MeanBefore - AOTAData.MeanDuring);
				num2 = (AOTAData.SDBefore - AOTAData.SDduring) / (AOTAData.MeanBefore - AOTAData.MeanDuring);
				for (int i = AOTAData.X0; i <= AOTAData.X3; i++)
				{
					if (!((i < 0) | (i >= AOTAData.StarCountToPlot)))
					{
						RawSignal[i - AOTAData.X0] = AOTAData.StarToPlot[i];
						ValidForComparison[i - AOTAData.X0] = AOTAData.StarValidToPlot[i];
						if (AOTAData.PlotForm.optChi2StdDevn.get_Checked())
						{
							RawSignal_SD[i - AOTAData.X0] = AOTAData.SDduring + ((double)AOTAData.StarToPlot[i] - AOTAData.MeanDuring) * num2;
						}
						else
						{
							RawSignal_SD[i - AOTAData.X0] = Math.Sqrt(Math.Abs(AOTAData.VarianceDuring + ((double)AOTAData.StarToPlot[i] - AOTAData.MeanDuring) * num));
						}
						if (RawSignal_SD[i - AOTAData.X0] == 0.0)
						{
							RawSignal_SD[i - AOTAData.X0] = 1.0;
						}
					}
				}
				return;
			}
			num = (AOTAData.VarianceAfter - AOTAData.VarianceDuring) / (AOTAData.MeanAfter - AOTAData.MeanDuring);
			num2 = (AOTAData.SDAfter - AOTAData.SDduring) / (AOTAData.MeanAfter - AOTAData.MeanDuring);
			for (int j = AOTAData.X4; j <= AOTAData.X7; j++)
			{
				if (!((j < 0) | (j >= AOTAData.StarCountToPlot)))
				{
					RawSignal[j - AOTAData.X4] = AOTAData.StarToPlot[j];
					ValidForComparison[j - AOTAData.X4] = AOTAData.StarValidToPlot[j];
					if (AOTAData.PlotForm.optChi2StdDevn.get_Checked())
					{
						RawSignal_SD[j - AOTAData.X4] = AOTAData.SDduring + ((double)AOTAData.StarToPlot[j] - AOTAData.MeanDuring) * num2;
					}
					else
					{
						RawSignal_SD[j - AOTAData.X4] = Math.Sqrt(Math.Abs(AOTAData.VarianceDuring + ((double)AOTAData.StarToPlot[j] - AOTAData.MeanDuring) * num));
					}
					if (RawSignal_SD[j - AOTAData.X4] == 0.0)
					{
						RawSignal_SD[j - AOTAData.X4] = 1.0;
					}
				}
			}
		}

		internal static void CreateObservedSignalArrayFor_MonteCarlo_Analysis(bool ForChi2)
		{
			for (int i = 0; i < LengthOfSignal; i++)
			{
				if (ForChi2 & !AOTAData.PlotForm.optMonteCarloTestSignal.get_Checked())
				{
					SignalForComparison[i] = Gaussian.BoxMuller(RawSignal[i], RawSignal_SD[i]);
				}
				else
				{
					SignalForComparison[i] = RawSignal[i];
				}
			}
		}

		internal static void CreateTestSignalArrayFor_MonteCarlo_Analysis(bool forMonteCarlo, bool Disappear)
		{
			if (forMonteCarlo & AOTAData.PlotForm.optMonteCarloTestSignal.get_Checked())
			{
				double num;
				double num2;
				if (Disappear)
				{
					num = (AOTAData.VarianceBefore - AOTAData.VarianceDuring) / (AOTAData.MeanBefore - AOTAData.MeanDuring);
					num2 = (AOTAData.SDBefore - AOTAData.SDduring) / (AOTAData.MeanBefore - AOTAData.MeanDuring);
				}
				else
				{
					num = (AOTAData.VarianceAfter - AOTAData.VarianceDuring) / (AOTAData.MeanAfter - AOTAData.MeanDuring);
					num2 = (AOTAData.SDAfter - AOTAData.SDduring) / (AOTAData.MeanBefore - AOTAData.MeanDuring);
				}
				for (int i = 0; i < LengthOfSignal; i++)
				{
					if (AOTAData.PlotForm.optChi2StdDevn.get_Checked())
					{
						TestForComparison[i] = Test[i] + (AOTAData.SDduring + (Test[i] - AOTAData.MeanDuring) * num2) * NoiseForTest[i];
					}
					else
					{
						TestForComparison[i] = Test[i] + Math.Sqrt(Math.Abs(AOTAData.VarianceDuring + (Test[i] - AOTAData.MeanDuring) * num)) * NoiseForTest[i];
					}
				}
			}
			else
			{
				for (int j = 0; j < LengthOfSignal; j++)
				{
					TestForComparison[j] = Test[j];
				}
			}
		}

		internal static void CreateNoise_TestSignal_MonteCarlo()
		{
			for (int i = 0; i < LengthOfSignal; i++)
			{
				NoiseForTest[i] = Gaussian.BoxMuller(0.0, 1.0);
			}
		}

		internal static double Chi_Squared(bool forMonteCarlo, bool Disappear)
		{
			double num = 0.0;
			double num2 = 0.0;
			int num3 = 0;
			CreateTestSignalArrayFor_MonteCarlo_Analysis(forMonteCarlo, Disappear);
			for (int i = 0; i < LengthOfSignal; i++)
			{
				if (ValidForComparison[i])
				{
					num2 = SignalForComparison[i] - TestForComparison[i];
					num += num2 * num2 / RawSignal_SD[i] / RawSignal_SD[i];
					num3++;
				}
			}
			if (num3 < 1)
			{
				return 1000.0;
			}
			return num / (double)num3;
		}

		internal static void Get_Chi2(bool Disappear)
		{
			double num = 0.0;
			double num2 = 1000.0;
			int num3 = 0;
			if (Disappear)
			{
				AOTAData.MinimumChi2DValue = 1000.0;
				AOTAData.MinimumChi2DPosition = 0;
				LengthOfSignal = AOTAData.X3 - AOTAData.X0 + 1;
				int num4 = AOTAData.X2 - AOTAData.X1;
				Chi2_D.Clear();
				Chi2_D_MinimumPosns = new int[AOTAData.MaximimumTransitions + 1];
				Chi2_D_Minimums = new double[AOTAData.MaximimumTransitions + 1];
				for (int i = 0; i <= AOTAData.MaximimumTransitions; i++)
				{
					Chi2_MinimaPositions.Index = i;
					num2 = 10000.0;
					for (int j = 0; j < num4; j++)
					{
						CreateTestSignalArray(i, Disappear, j);
						TransferObservedDataToAnalyse(Disappear);
						CreateObservedSignalArrayFor_MonteCarlo_Analysis(ForChi2: false);
						CreateTestSignalArrayFor_MonteCarlo_Analysis(forMonteCarlo: false, Disappear);
						num = Chi_Squared(forMonteCarlo: false, Disappear);
						if (i == 0)
						{
							Chi2Entry = new Chi2_MinimaPositions();
							Chi2Entry.Pos = j + AOTAData.X1;
							Chi2Entry.Chi2_value = num;
							Chi2_D.Add(Chi2Entry);
						}
						else
						{
							Chi2_D[j].Chi2_value = num;
						}
						if (num < num2)
						{
							num2 = num;
							num3 = j + AOTAData.X1;
						}
					}
					Chi2_D_Minimums[i] = num2;
					Chi2_D_MinimumPosns[i] = num3;
					if (num2 < AOTAData.MinimumChi2DValue)
					{
						AOTAData.MinimumChi2DValue = num2;
						AOTAData.MinimumChi2DPosition = i;
					}
				}
			}
			else
			{
				AOTAData.MinimumChi2RValue = 1000.0;
				AOTAData.MinimumChi2RPosition = 0;
				LengthOfSignal = AOTAData.X7 - AOTAData.X4 + 1;
				int num4 = AOTAData.X6 - AOTAData.X5;
				Chi2_R.Clear();
				Chi2_R_MinimumPosns = new int[AOTAData.MaximimumTransitions + 1];
				Chi2_R_Minimums = new double[AOTAData.MaximimumTransitions + 1];
				for (int k = 0; k <= AOTAData.MaximimumTransitions; k++)
				{
					Chi2_MinimaPositions.Index = k;
					num2 = 10000.0;
					for (int l = 0; l < num4; l++)
					{
						CreateTestSignalArray(k, Disappear, l);
						TransferObservedDataToAnalyse(Disappear);
						CreateObservedSignalArrayFor_MonteCarlo_Analysis(ForChi2: false);
						CreateTestSignalArrayFor_MonteCarlo_Analysis(forMonteCarlo: false, Disappear);
						num = Chi_Squared(forMonteCarlo: false, Disappear);
						if (k == 0)
						{
							Chi2Entry = new Chi2_MinimaPositions();
							Chi2Entry.Pos = l + AOTAData.X5;
							Chi2Entry.Chi2_value = num;
							Chi2_R.Add(Chi2Entry);
						}
						else
						{
							Chi2_R[l].Chi2_value = num;
						}
						if (num < num2)
						{
							num2 = num;
							num3 = l + AOTAData.X5;
						}
					}
					Chi2_R_Minimums[k] = num2;
					Chi2_R_MinimumPosns[k] = num3;
					if (num2 < AOTAData.MinimumChi2RValue)
					{
						AOTAData.MinimumChi2RValue = num2;
						AOTAData.MinimumChi2RPosition = k;
					}
				}
			}
			AOTAData.PlotAOTA();
			AOTAData.PlotChi2();
		}

		internal static void GetUncertainty(int Slope, bool Disappear)
		{
			double num = 1000.0;
			int num2 = 0;
			double num3 = 0.0;
			int num4 = 10;
			if (Slope < 0)
			{
				return;
			}
			int num5;
			int num6;
			if (Disappear)
			{
				LengthOfSignal = AOTAData.X3 - AOTAData.X0 + 1;
				num5 = AOTAData.X2 - AOTAData.X1;
				num6 = Chi2_D_MinimumPosns[Slope] - AOTAData.X1;
				Array.Clear(Chi2_D_CMcounts, 0, 21);
			}
			else
			{
				LengthOfSignal = AOTAData.X7 - AOTAData.X4 + 1;
				num5 = AOTAData.X6 - AOTAData.X5;
				num6 = Chi2_R_MinimumPosns[Slope] - AOTAData.X5;
				Array.Clear(Chi2_R_CMcounts, 0, 21);
			}
			if (LengthOfSignal < 2)
			{
				return;
			}
			for (int i = 0; i < MonteCarloTrials; i++)
			{
				num = 1000.0;
				NoiseForTest = new double[LengthOfSignal];
				CreateNoise_TestSignal_MonteCarlo();
				for (int j = 0; j < num5; j++)
				{
					CreateTestSignalArray(Slope, Disappear, j);
					CreateTestSignalArrayFor_MonteCarlo_Analysis(forMonteCarlo: true, Disappear);
					TransferObservedDataToAnalyse(Disappear);
					CreateObservedSignalArrayFor_MonteCarlo_Analysis(ForChi2: true);
					num3 = Chi_Squared(forMonteCarlo: true, Disappear);
					if (num3 < num)
					{
						num = num3;
						num2 = j;
					}
				}
				num4 = num2 - num6;
				if (num4 < -10)
				{
					num4 = -10;
				}
				if (num4 > 10)
				{
					num4 = 10;
				}
				if (Disappear)
				{
					Chi2_D_CMcounts[10 + num4]++;
				}
				else
				{
					Chi2_R_CMcounts[10 + num4]++;
				}
			}
			AOTAData.PlotUncertainties();
		}
	}
}
