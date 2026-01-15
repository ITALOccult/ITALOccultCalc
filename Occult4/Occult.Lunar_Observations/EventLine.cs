using System;
using System.Text;
using Occult.Properties;

namespace Occult.Lunar_Observations
{
	internal class EventLine : IComparable
	{
		public static int SortField;

		public static bool UseOldFormat;

		private int seqNumber;

		private int year;

		private int month;

		private int day;

		private int hour;

		private int minute;

		private int phase_old;

		private int starNumber;

		private int certainty;

		private int stability;

		private int transparency;

		private int circumstances;

		private int temperature;

		private int grazeFlag_old;

		private double second;

		private double pE;

		private double accuracy;

		private double signalToNoise;

		private double duration;

		private int second_DecPlaces;

		private int pE_DecPlaces;

		private int accuracy_DecPlaces;

		private int signalToNoise_DecPlaces;

		private int duration_DecPlaces;

		private bool signalToNoise_Valid;

		private bool temperature_Valid;

		private bool accuracy_Valid;

		private bool pE_Valid;

		private bool duration_Valid;

		private bool pe_AdjustIfBrightStar;

		private string starCat;

		private string station = "     ";

		private string telescope = "  ";

		private string observer = "  ";

		private string recorder = "  ";

		private string methodRecording1 = " ";

		private string methodRecording2 = " ";

		private string methodTiming = " ";

		private string pEApplication = " ";

		private string doubleStarComponent = " ";

		private string eventTelescope = " ";

		private string eventObserver = " ";

		private string eventRecorder = " ";

		private string comments = "";

		private string occevent = "D";

		private string limb = "D";

		private string grazeflag = " ";

		private string wds = " ";

		private string lightLevel = " ";

		public int SeqNumber
		{
			get
			{
				return seqNumber;
			}
			set
			{
				seqNumber = value;
			}
		}

		public int Year
		{
			get
			{
				return year;
			}
			set
			{
				year = value;
			}
		}

		public int Month
		{
			get
			{
				return month;
			}
			set
			{
				month = value;
			}
		}

		public int Day
		{
			get
			{
				return day;
			}
			set
			{
				day = value;
			}
		}

		public int Hour
		{
			get
			{
				return hour;
			}
			set
			{
				hour = value;
			}
		}

		public int Minute
		{
			get
			{
				return minute;
			}
			set
			{
				minute = value;
			}
		}

		public double Second
		{
			get
			{
				return second;
			}
			set
			{
				second = value;
			}
		}

		public int Second_DecPlaces
		{
			get
			{
				return second_DecPlaces;
			}
			set
			{
				second_DecPlaces = value;
			}
		}

		public double EventJD0Hrs => Utilities.JD_from_Date(year, month, day);

		public double EventTime_inHours => (double)hour + (double)minute / 60.0 + second / 3600.0;

		public double EventJDTime => Utilities.JD_from_Date(year, month, day) + ((double)hour + (double)minute / 60.0 + second / 3600.0) / 24.0;

		public double EventJDTime_PEcorrected => EventDate_PEcorrected();

		public bool PE_AdjustIfBrightStar => pe_AdjustIfBrightStar;

		public int StarNumber
		{
			get
			{
				return starNumber;
			}
			set
			{
				starNumber = value;
			}
		}

		public int Phase_Old
		{
			get
			{
				return phase_old;
			}
			set
			{
				phase_old = value;
			}
		}

		public string OccEvent
		{
			get
			{
				return occevent;
			}
			set
			{
				occevent = value;
			}
		}

		public string Limb
		{
			get
			{
				return limb;
			}
			set
			{
				limb = value;
			}
		}

		public string GrazeFlag
		{
			get
			{
				return grazeflag;
			}
			set
			{
				grazeflag = value;
			}
		}

		public string WDS
		{
			get
			{
				return wds;
			}
			set
			{
				wds = value;
			}
		}

		public int Certainty
		{
			get
			{
				return certainty;
			}
			set
			{
				certainty = value;
			}
		}

		public int Stability
		{
			get
			{
				return stability;
			}
			set
			{
				stability = value;
			}
		}

		public int Transparency
		{
			get
			{
				return transparency;
			}
			set
			{
				transparency = value;
			}
		}

		public int Circumstances
		{
			get
			{
				return circumstances;
			}
			set
			{
				circumstances = value;
			}
		}

		public int Temperature
		{
			get
			{
				return temperature;
			}
			set
			{
				temperature = value;
			}
		}

		public bool Temperature_Valid
		{
			get
			{
				return temperature_Valid;
			}
			set
			{
				temperature_Valid = value;
			}
		}

		public double PE
		{
			get
			{
				return pE;
			}
			set
			{
				pE = value;
			}
		}

		public int PE_DecPlaces
		{
			get
			{
				return pE_DecPlaces;
			}
			set
			{
				pE_DecPlaces = value;
			}
		}

		public bool PE_Valid
		{
			get
			{
				return pE_Valid;
			}
			set
			{
				pE_Valid = value;
			}
		}

		public double Accuracy
		{
			get
			{
				return accuracy;
			}
			set
			{
				accuracy = value;
			}
		}

		public int Accuracy_DecPlaces
		{
			get
			{
				return accuracy_DecPlaces;
			}
			set
			{
				accuracy_DecPlaces = value;
			}
		}

		public bool Accuracy_Valid
		{
			get
			{
				return accuracy_Valid;
			}
			set
			{
				accuracy_Valid = value;
			}
		}

		public double SignalToNoise
		{
			get
			{
				return signalToNoise;
			}
			set
			{
				signalToNoise = value;
			}
		}

		public int SignalToNoise_DecPlaces
		{
			get
			{
				return signalToNoise_DecPlaces;
			}
			set
			{
				signalToNoise_DecPlaces = value;
			}
		}

		public bool SignalToNoise_Valid
		{
			get
			{
				return signalToNoise_Valid;
			}
			set
			{
				signalToNoise_Valid = value;
			}
		}

		public double Duration
		{
			get
			{
				return duration;
			}
			set
			{
				duration = value;
			}
		}

		public int Duration_DecPlaces
		{
			get
			{
				return duration_DecPlaces;
			}
			set
			{
				duration_DecPlaces = value;
			}
		}

		public bool Duration_Valid
		{
			get
			{
				return duration_Valid;
			}
			set
			{
				duration_Valid = value;
			}
		}

		public string LightLevel
		{
			get
			{
				return lightLevel;
			}
			set
			{
				lightLevel = value;
			}
		}

		public string StarCat
		{
			get
			{
				return starCat;
			}
			set
			{
				starCat = value;
			}
		}

		public string Station
		{
			get
			{
				return station;
			}
			set
			{
				station = value;
			}
		}

		public string Telescope
		{
			get
			{
				return telescope;
			}
			set
			{
				telescope = value;
			}
		}

		public string Observer
		{
			get
			{
				return observer;
			}
			set
			{
				observer = value;
			}
		}

		public string Recorder
		{
			get
			{
				return recorder;
			}
			set
			{
				recorder = value;
			}
		}

		public string MethodRecording1
		{
			get
			{
				return methodRecording1;
			}
			set
			{
				methodRecording1 = value;
			}
		}

		public string MethodRecording2
		{
			get
			{
				return methodRecording2;
			}
			set
			{
				methodRecording2 = value;
			}
		}

		public string MethodTiming
		{
			get
			{
				return methodTiming;
			}
			set
			{
				methodTiming = value;
			}
		}

		public string PEApplication
		{
			get
			{
				return pEApplication;
			}
			set
			{
				pEApplication = value;
			}
		}

		public string DoubleStarComponent
		{
			get
			{
				return doubleStarComponent;
			}
			set
			{
				doubleStarComponent = value;
			}
		}

		public int GrazeFlag_old
		{
			get
			{
				return grazeFlag_old;
			}
			set
			{
				grazeFlag_old = value;
			}
		}

		public string EventTelescope
		{
			get
			{
				return eventTelescope;
			}
			set
			{
				eventTelescope = value;
			}
		}

		public string EventObserver
		{
			get
			{
				return eventObserver;
			}
			set
			{
				eventObserver = value;
			}
		}

		public string EventRecorder
		{
			get
			{
				return eventRecorder;
			}
			set
			{
				eventRecorder = value;
			}
		}

		public string Comments
		{
			get
			{
				return comments;
			}
			set
			{
				comments = value;
			}
		}

		public void DecodeObservationLine_Occult(string ELine1, string ELine2)
		{
			string text = ELine1.PadRight(80);
			string text2 = ELine2.PadRight(80);
			if (ELine1.Length < 70)
			{
				if (!int.TryParse(text.Substring(0, 4), out year))
				{
					year = 0;
				}
				else if ((year >= 0) & (year < 100))
				{
					year += 2000;
				}
				if (!int.TryParse(text.Substring(4, 2), out month))
				{
					month = 1;
				}
				if (!int.TryParse(text.Substring(6, 2), out day))
				{
					day = 1;
				}
				if (!int.TryParse(text.Substring(8, 2), out hour))
				{
					hour = 0;
				}
				if (!int.TryParse(text.Substring(10, 2), out minute))
				{
					minute = 0;
				}
				if (!double.TryParse(text.Substring(12, 6), out second))
				{
					second = 0.0;
				}
				second_DecPlaces = Utilities.DecimalPlaces(text.Substring(12, 6));
				starCat = text.Substring(18, 1);
				if ((StarCat == " ") & (text2.Substring(4, 1) == "G"))
				{
					starCat = "G";
					if (!int.TryParse(text2.Substring(5, 9), out starNumber))
					{
						starNumber = 0;
					}
					comments = text2.Substring(14).Trim();
				}
				else
				{
					if (!int.TryParse(text.Substring(19, 6), out starNumber))
					{
						starNumber = 0;
					}
					comments = text2.Substring(4).Trim();
				}
				if ((starCat == "R") & (starNumber > 4000))
				{
					if (starNumber < 4010)
					{
						starNumber = (starNumber - 4000) * 1000;
					}
					else if (starNumber < 4100)
					{
						int num = starNumber % 100;
						int num2 = starNumber % 10;
						starNumber = 100 * (num - num2) + num2;
					}
				}
				wds = text.Substring(25, 1);
				occevent = text.Substring(26, 1).ToUpper();
				limb = text.Substring(27, 1).ToUpper();
				grazeflag = text.Substring(28, 1).ToUpper();
				if (occevent == "D")
				{
					if (Limb == "E")
					{
						phase_old = 5;
					}
					else if (Limb == "B")
					{
						phase_old = 3;
					}
					else
					{
						phase_old = 1;
					}
				}
				else if (occevent == "R")
				{
					if (Limb == "E")
					{
						phase_old = 6;
					}
					else if (Limb == "B")
					{
						phase_old = 4;
					}
					else
					{
						phase_old = 2;
					}
				}
				else if (OccEvent == "B")
				{
					phase_old = 7;
				}
				else if (OccEvent == "F")
				{
					phase_old = 8;
				}
				else if (OccEvent == "M")
				{
					phase_old = 9;
				}
				if (grazeflag == "G")
				{
					grazeFlag_old = 6;
				}
				if (!double.TryParse(text.Substring(29, 4), out pE))
				{
					pE = 0.0;
				}
				pE_DecPlaces = Utilities.DecimalPlaces(text.Substring(29, 4));
				pE_Valid = text.Substring(29, 4).Trim().Length > 0;
				pEApplication = text.Substring(33, 1).ToUpper();
				if (pEApplication == "N")
				{
					pEApplication = "X";
				}
				methodRecording1 = text.Substring(34, 1).ToUpper();
				methodRecording2 = text.Substring(35, 1).ToUpper();
				methodTiming = text.Substring(36, 1).ToUpper();
				if (!double.TryParse(text.Substring(37, 5), out accuracy))
				{
					accuracy = 0.0;
				}
				accuracy_DecPlaces = Utilities.DecimalPlaces(text.Substring(37, 5));
				accuracy_Valid = text.Substring(37, 5).Trim().Length > 0;
				if (!int.TryParse(text.Substring(42, 1), out certainty))
				{
					certainty = 0;
				}
				if (!double.TryParse(text.Substring(43, 3), out signalToNoise))
				{
					signalToNoise = 0.0;
				}
				signalToNoise_DecPlaces = Utilities.DecimalPlaces(text.Substring(43, 3));
				signalToNoise_Valid = text.Substring(43, 3).Trim().Length > 0;
				doubleStarComponent = text.Substring(46, 1).ToUpper();
				if (!double.TryParse(text.Substring(47, 5), out duration))
				{
					duration = 0.0;
				}
				duration_DecPlaces = Utilities.DecimalPlaces(text.Substring(47, 5));
				duration_Valid = text.Substring(47, 5).Trim().Length > 0;
				lightLevel = text.Substring(52, 1).ToUpper();
				if (!int.TryParse(text.Substring(53, 1), out stability))
				{
					stability = 0;
				}
				if (!int.TryParse(text.Substring(54, 1), out transparency))
				{
					transparency = 0;
				}
				if (!int.TryParse(text.Substring(55, 1), out circumstances))
				{
					circumstances = 0;
				}
				if (!int.TryParse(text.Substring(56, 3).Replace("- ", "-"), out temperature))
				{
					temperature = 0;
				}
				temperature_Valid = text.Substring(56, 3).Trim().Length > 0;
				eventTelescope = text.Substring(59, 1);
				eventObserver = text.Substring(60, 1);
				eventRecorder = eventObserver;
			}
			else
			{
				if (text.Substring(73, 1) != " ")
				{
					text = text.Insert(72, "  ");
					text = text.Insert(2, Settings.Default.ReportCentury.ToString());
				}
				else if (text.Substring(75, 1) != " ")
				{
					text = text.Insert(2, Settings.Default.ReportCentury.ToString());
				}
				if (!int.TryParse(text.Substring(0, 2), out seqNumber))
				{
					seqNumber = 1;
				}
				if (!int.TryParse(text.Substring(2, 4), out year))
				{
					year = 0;
				}
				else if ((year >= 0) & (year < 100))
				{
					year += 2000;
				}
				if (!int.TryParse(text.Substring(6, 2), out month))
				{
					month = 1;
				}
				if (!int.TryParse(text.Substring(8, 2), out day))
				{
					day = 1;
				}
				if (!int.TryParse(text.Substring(10, 2), out hour))
				{
					hour = 0;
				}
				if (!int.TryParse(text.Substring(12, 2), out minute))
				{
					minute = 0;
				}
				if (!double.TryParse(text.Substring(14, 5).Insert(2, ".").Replace("..", "."), out second))
				{
					second = 0.0;
				}
				second_DecPlaces = Utilities.DecimalPlaces(text.Substring(14, 5).Insert(2, "."));
				starCat = text.Substring(19, 1);
				if ((StarCat == " ") & (text2.Substring(4, 1) == "G"))
				{
					starCat = "G";
					if (!int.TryParse(text2.Substring(5, 9), out starNumber))
					{
						starNumber = 0;
					}
					comments = text2.Substring(14).Trim();
				}
				else
				{
					if (!int.TryParse(text.Substring(20, 7), out starNumber))
					{
						starNumber = 0;
					}
					comments = text2.Substring(4).Trim();
				}
				if ((starCat == "R") & (starNumber > 4000))
				{
					starCat = "P";
					if (starNumber < 4010)
					{
						starNumber = (starNumber - 4000) * 1000;
					}
					else if (starNumber < 4100)
					{
						int num = starNumber % 100;
						int num2 = starNumber % 10;
						starNumber = 100 * (num - num2) + num2;
					}
				}
				station = text.Substring(27, 5);
				telescope = text.Substring(32, 2);
				observer = text.Substring(34, 2);
				recorder = text.Substring(36, 2);
				if (!int.TryParse(text.Substring(38, 1), out phase_old))
				{
					phase_old = 0;
				}
				if ((phase_old == 1) | (phase_old == 3) | (phase_old == 5))
				{
					occevent = "D";
				}
				else if ((phase_old == 2) | (phase_old == 4) | (phase_old == 6))
				{
					occevent = "R";
				}
				else if (phase_old == 7)
				{
					occevent = "B";
				}
				else if (phase_old == 8)
				{
					occevent = "F";
				}
				else if (phase_old == 9)
				{
					occevent = "M";
				}
				if ((phase_old == 1) | (phase_old == 2))
				{
					limb = "D";
				}
				else if ((phase_old == 3) | (phase_old == 4))
				{
					limb = "B";
				}
				else if ((phase_old == 5) | (phase_old == 6))
				{
					limb = "U";
				}
				methodRecording1 = text.Substring(39, 1);
				methodRecording2 = text.Substring(40, 1);
				methodTiming = text.Substring(41, 1);
				pEApplication = text.Substring(42, 1);
				if (pEApplication == "N")
				{
					pEApplication = "X";
				}
				if (!double.TryParse(text.Substring(43, 2).Insert(0, "."), out pE))
				{
					pE = 0.0;
				}
				pE_DecPlaces = Utilities.DecimalPlaces(text.Substring(43, 2).Insert(0, "."));
				pE_Valid = text.Substring(43, 2).Trim().Length > 0;
				if (!double.TryParse(text.Substring(45, 3).Insert(1, "."), out accuracy))
				{
					accuracy = 0.0;
				}
				accuracy_DecPlaces = Utilities.DecimalPlaces(text.Substring(45, 3).Insert(1, "."));
				accuracy_Valid = text.Substring(45, 3).Trim().Length > 0;
				if (!int.TryParse(text.Substring(48, 1), out certainty))
				{
					certainty = 1;
				}
				if (!double.TryParse(text.Substring(49, 2).Insert(1, "."), out signalToNoise))
				{
					signalToNoise = 0.0;
				}
				signalToNoise_DecPlaces = Utilities.DecimalPlaces(text.Substring(49, 2).Insert(1, "."));
				signalToNoise_Valid = text.Substring(49, 2).Trim().Length > 0;
				doubleStarComponent = text.Substring(51, 1);
				if (!int.TryParse(text.Substring(52, 1), out stability))
				{
					stability = 0;
				}
				if (!int.TryParse(text.Substring(53, 1), out transparency))
				{
					transparency = 0;
				}
				if (!int.TryParse(text.Substring(54, 1), out circumstances))
				{
					circumstances = 0;
				}
				duration = 0.0;
				duration_Valid = false;
				lightLevel = " ";
				if (!int.TryParse(text.Substring(55, 2), out temperature))
				{
					temperature = 0;
				}
				temperature_Valid = text.Substring(55, 2).Trim().Length > 0;
				if (!int.TryParse(text.Substring(59, 1), out grazeFlag_old))
				{
					grazeFlag_old = 0;
				}
				if (grazeFlag_old < 6)
				{
					GrazeFlag = " ";
				}
				else if (grazeFlag_old >= 6)
				{
					GrazeFlag = "G";
				}
				if (grazeFlag_old == 7)
				{
					occevent = "O";
				}
				else if (grazeFlag_old == 8)
				{
					occevent = "S";
				}
				else if (grazeFlag_old == 9)
				{
					occevent = "E";
				}
				eventTelescope = text.Substring(77, 1);
				eventObserver = text.Substring(78, 1);
				eventRecorder = text.Substring(79, 1);
			}
			if (starCat == "S")
			{
				if (XZ80Q.Get_SAO_Star(starNumber) && XZ80Q.ZC > 0)
				{
					starNumber = XZ80Q.ZC;
					starCat = "R";
				}
			}
			else if (starCat == "X")
			{
				XZ80Q.Get_XZ_Star(starNumber);
				if (XZ80Q.ZC > 0)
				{
					starNumber = XZ80Q.ZC;
					starCat = "R";
				}
				else if (XZ80Q.SAO > 0)
				{
					starNumber = XZ80Q.SAO;
					starCat = "S";
				}
			}
			if (methodRecording1 == " ")
			{
				methodRecording1 = methodRecording2;
				methodRecording2 = " ";
			}
		}

		public override string ToString()
		{
			GetEventLines(out var Line, out var Line2);
			if (Line2.Trim().Length > 0)
			{
				return Line + "\r\n" + Line2;
			}
			return Line;
		}

		public void GetEventLines(out string Line1, out string Line2)
		{
			StringBuilder stringBuilder = new StringBuilder();
			StringBuilder stringBuilder2 = new StringBuilder();
			stringBuilder2.Append("    ");
			if (UseOldFormat)
			{
				stringBuilder.Append($"{seqNumber:F0}".PadLeft(4, '0').Substring(2));
			}
			stringBuilder.AppendFormat("{0,4:F0}", year);
			stringBuilder.AppendFormat("{0,2:F0}", month);
			stringBuilder.AppendFormat("{0,2:F0}", day);
			stringBuilder.AppendFormat("{0,2:F0}", hour);
			stringBuilder.AppendFormat("{0,2:F0}", minute);
			if (UseOldFormat)
			{
				stringBuilder.Append(Utilities.FormatNumber(second, 2, second_DecPlaces).PadRight(6).Remove(2, 1));
			}
			else
			{
				stringBuilder.Append(Utilities.FormatNumber(second, 2, second_DecPlaces).PadRight(6));
			}
			if (UseOldFormat)
			{
				if (starCat == "G")
				{
					stringBuilder.Append("".PadRight(8));
					stringBuilder2.Append("G");
					stringBuilder2.AppendFormat("{0,9:F0}", starNumber);
				}
				else
				{
					stringBuilder.Append(starCat.PadRight(1).Substring(0, 1));
					stringBuilder.AppendFormat("{0,7:F0}", starNumber);
				}
				stringBuilder.Append(station.Trim().PadRight(5).Substring(0, 5));
				stringBuilder.Append(telescope.Trim().PadRight(2).Substring(0, 2));
				stringBuilder.Append(observer.Trim().PadRight(2).Substring(0, 2));
				stringBuilder.Append(recorder.Trim().PadRight(2).Substring(0, 2));
				stringBuilder.AppendFormat("{0:F0}", phase_old);
				if (methodRecording1 == "G")
				{
					stringBuilder.Append("V");
				}
				else
				{
					stringBuilder.Append(methodRecording1);
				}
				if (methodRecording2 == "G")
				{
					stringBuilder.Append("V");
				}
				else
				{
					stringBuilder.Append(methodRecording2);
				}
				if (methodTiming == "G")
				{
					stringBuilder.Append("R");
				}
				else if (methodTiming == "N")
				{
					stringBuilder.Append("C");
				}
				else if (methodTiming == "O")
				{
					stringBuilder.Append("M");
				}
				else
				{
					stringBuilder.Append(methodTiming);
				}
				stringBuilder.Append(pEApplication);
				if (pE_Valid)
				{
					stringBuilder.Append(Utilities.FormatNumber(pE, 1, pE_DecPlaces).Substring(2).PadRight(2));
				}
				else
				{
					stringBuilder.Append("  ");
				}
				if (accuracy_Valid)
				{
					if ((year >= 1600) & (year <= 1750))
					{
						string text = string.Format("{0,4:F1}", accuracy).Remove(2, 1);
						if ((text.Substring(0, 2) == " 0") & (accuracy != 0.0))
						{
							text = "  " + text.Substring(2);
						}
						if (accuracy_DecPlaces == 1)
						{
							stringBuilder.Append(text);
						}
						else
						{
							stringBuilder.Append(text.Substring(0, 2) + " ");
						}
					}
					else
					{
						string text = string.Format("{0,4:F2}", accuracy).Remove(1, 1);
						if ((text.Substring(0, 1) == "0") & (accuracy != 0.0))
						{
							text = " " + text.Substring(1);
						}
						if (accuracy_DecPlaces > 1)
						{
							stringBuilder.Append(text);
						}
						else if (accuracy_DecPlaces == 1)
						{
							stringBuilder.Append(text.Substring(0, 2) + " ");
						}
						else
						{
							stringBuilder.Append(text.Substring(0, 1) + "  ");
						}
					}
				}
				else
				{
					stringBuilder.Append("   ");
				}
				stringBuilder.AppendFormat("{0,1:F0}", certainty);
				if (signalToNoise_Valid)
				{
					if (signalToNoise > 9.9)
					{
						signalToNoise = 9.9;
					}
					string text = string.Format("{0,3:F1}", signalToNoise).Remove(1, 1);
					if (signalToNoise_DecPlaces > 0)
					{
						stringBuilder.Append(text);
					}
					else
					{
						stringBuilder.Append(text.Substring(0, 1) + " ");
					}
				}
				else
				{
					stringBuilder.Append("  ");
				}
				stringBuilder.Append(doubleStarComponent.PadRight(1).Substring(0, 1));
				if (stability > 0)
				{
					stringBuilder.AppendFormat("{0,1:F0}", stability);
				}
				else
				{
					stringBuilder.Append(" ");
				}
				if (transparency > 0)
				{
					stringBuilder.AppendFormat("{0,1:F0}", transparency);
				}
				else
				{
					stringBuilder.Append(" ");
				}
				if (circumstances == 9)
				{
					stringBuilder.Append("8");
				}
				else if (circumstances > 0)
				{
					stringBuilder.AppendFormat("{0,1:F0}", circumstances);
				}
				else
				{
					stringBuilder.Append(" ");
				}
				if (temperature_Valid & ((double)temperature > -9.5))
				{
					stringBuilder.AppendFormat("{0,2:F0}", temperature);
				}
				else
				{
					stringBuilder.Append("  ");
				}
				stringBuilder.Append("  ");
				if (grazeFlag_old > 5)
				{
					stringBuilder.AppendFormat("{0,1:F0}", grazeFlag_old);
				}
				else
				{
					stringBuilder.Append(" ");
				}
				stringBuilder.Append("".PadRight(17));
				stringBuilder.Append(eventTelescope.PadRight(1).Substring(0, 1));
				stringBuilder.Append(eventObserver.PadRight(1).Substring(0, 1));
				stringBuilder.Append(eventRecorder.PadRight(1).Substring(0, 1));
			}
			else
			{
				if (starCat == "G")
				{
					stringBuilder.Append("".PadRight(7));
					stringBuilder2.Append("G");
					stringBuilder2.AppendFormat("{0,9:F0}", starNumber);
				}
				else
				{
					stringBuilder.Append(starCat.PadRight(1).Substring(0, 1));
					stringBuilder.AppendFormat("{0,6:F0}", starNumber);
				}
				stringBuilder.Append(wds.PadRight(1).Substring(0, 1));
				stringBuilder.Append(occevent.PadRight(1).Substring(0, 1));
				stringBuilder.Append(limb.PadRight(1).Substring(0, 1));
				stringBuilder.Append(grazeflag.PadRight(1).Substring(0, 1));
				if (pE_Valid)
				{
					stringBuilder.Append(Utilities.FormatNumber(pE, 1, pE_DecPlaces).PadRight(4));
				}
				else
				{
					stringBuilder.Append("    ");
				}
				stringBuilder.Append(pEApplication);
				stringBuilder.Append(methodRecording1);
				stringBuilder.Append(methodRecording2);
				stringBuilder.Append(methodTiming);
				if (accuracy_Valid)
				{
					if ((year >= 1600) & (year <= 1750))
					{
						string text = string.Format("{0,5:F2}", accuracy);
						if ((accuracy_DecPlaces > 0) & (accuracy < 10.0))
						{
							stringBuilder.Append(text.Substring(0, 4) + " ");
						}
						else
						{
							stringBuilder.Append(text.Substring(0, 3) + "  ");
						}
					}
					else
					{
						string text = string.Format("{0,5:F3}", accuracy);
						if (accuracy_DecPlaces > 2)
						{
							stringBuilder.Append(text);
						}
						else if (accuracy_DecPlaces == 2)
						{
							stringBuilder.Append(text.Substring(0, 4) + " ");
						}
						else if (accuracy_DecPlaces == 1)
						{
							stringBuilder.Append(text.Substring(0, 3) + "  ");
						}
						else
						{
							stringBuilder.Append(text.Substring(0, 2) + "   ");
						}
					}
				}
				else
				{
					stringBuilder.Append("     ");
				}
				stringBuilder.AppendFormat("{0,1:F0}", certainty);
				if (signalToNoise_Valid)
				{
					if (signalToNoise > 9.9)
					{
						signalToNoise = 9.9;
					}
					string text = string.Format("{0,3:F1}", signalToNoise);
					if (signalToNoise_DecPlaces > 0)
					{
						stringBuilder.Append(text);
					}
					else
					{
						stringBuilder.Append(text.Substring(0, 2) + " ");
					}
				}
				else
				{
					stringBuilder.Append("   ");
				}
				stringBuilder.Append(doubleStarComponent.PadRight(1).Substring(0, 1));
				if (duration_Valid)
				{
					string text = string.Format("{0,5:F3}", duration);
					if (duration_DecPlaces > 2)
					{
						stringBuilder.Append(text);
					}
					else if (duration_DecPlaces == 2)
					{
						stringBuilder.Append(text.Substring(0, 4) + " ");
					}
					else if (duration_DecPlaces == 1)
					{
						stringBuilder.Append(text.Substring(0, 3) + "  ");
					}
					else
					{
						stringBuilder.Append(text.Substring(0, 2) + "   ");
					}
				}
				else
				{
					stringBuilder.Append("     ");
				}
				stringBuilder.Append(lightLevel.PadRight(1).Substring(0, 1));
				if (stability > 0)
				{
					stringBuilder.AppendFormat("{0,1:F0}", stability);
				}
				else
				{
					stringBuilder.Append(" ");
				}
				if (transparency > 0)
				{
					stringBuilder.AppendFormat("{0,1:F0}", transparency);
				}
				else
				{
					stringBuilder.Append(" ");
				}
				if (circumstances > 0)
				{
					stringBuilder.AppendFormat("{0,1:F0}", circumstances);
				}
				else
				{
					stringBuilder.Append(" ");
				}
				if (temperature_Valid & (temperature > -50))
				{
					stringBuilder.AppendFormat("{0,3:F0}", temperature);
				}
				else
				{
					stringBuilder.Append("   ");
				}
				stringBuilder.Append(eventTelescope.PadRight(1).Substring(0, 1));
				stringBuilder.Append(eventObserver.PadRight(1).Substring(0, 1));
			}
			stringBuilder2.Append(comments);
			Line1 = stringBuilder.ToString();
			Line2 = stringBuilder2.ToString();
		}

		public int CompareTo(object other)
		{
			switch (SortField)
			{
			case 0:
				if (SeqNumber == ((EventLine)other).SeqNumber)
				{
					return EventTime_inHours.CompareTo(((EventLine)other).EventTime_inHours);
				}
				return SeqNumber.CompareTo(((EventLine)other).SeqNumber);
			case 1:
				if (EventTelescope == ((EventLine)other).EventTelescope)
				{
					return EventTime_inHours.CompareTo(((EventLine)other).EventTime_inHours);
				}
				return EventTelescope.CompareTo(((EventLine)other).EventTelescope);
			case 2:
				if (EventObserver == ((EventLine)other).EventObserver)
				{
					return EventTime_inHours.CompareTo(((EventLine)other).EventTime_inHours);
				}
				return EventObserver.CompareTo(((EventLine)other).EventObserver);
			case 3:
				return EventJDTime.CompareTo(((EventLine)other).EventJDTime);
			default:
				return 0;
			}
		}

		private double EventDate_PEcorrected()
		{
			double num = 0.0;
			pe_AdjustIfBrightStar = false;
			if ((pEApplication == "U") & pE_Valid)
			{
				num = pE;
			}
			else if ((pEApplication == "X") | (pEApplication == " "))
			{
				num = ((!((phase_old == 1) | (phase_old == 3) | (phase_old == 5) | (phase_old == 7))) ? 0.99 : 0.48);
				pe_AdjustIfBrightStar = true;
			}
			return Utilities.JD_from_Date(year, month, day) + ((double)hour + (double)minute / 60.0 + (second - num) / 3600.0) / 24.0;
		}
	}
}
