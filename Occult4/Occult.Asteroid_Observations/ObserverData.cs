using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace Occult.Asteroid_Observations
{
	internal class ObserverData : IComparable
	{
		public static int SortField = 0;

		private int seqNumber;

		private string observer1 = "";

		private string observer2 = "";

		private string observersAll;

		private string nearTo = "";

		private string stateCountry = "";

		private bool moreThan2Observers;

		private bool longitude_SignIsPlus;

		private bool latitude_SignisPlus;

		private int longitude_Deg;

		private int longitude_Min;

		private int latitude_Deg;

		private int latitude_Min;

		private int altitude;

		private double longitude_Sec;

		private double latitude_Sec;

		private double telescopeAperture;

		private string datum = " ";

		private string telescopeType = " ";

		private string stability = " ";

		private string transparency = " ";

		private string timeSource = " ";

		private int h_Dis;

		private int m_Dis;

		private int weight_D;

		private double s_Dis;

		private double accuracy_D;

		private double pEQ_D;

		private string event_D = "D";

		private int h_Reap;

		private int m_Reap;

		private int weight_R;

		private double s_Reap;

		private double accuracy_R;

		private double pEQ_R;

		private string event_R = "R";

		private string method = " ";

		private string plotCode = " ";

		private double timeAdjustment;

		private string freeText = "";

		private bool altitude_Set;

		private bool accuracy_D_Set;

		private bool pEQ_D_Set;

		private bool accuracy_R_Set;

		private bool pEQ_R_Set;

		private bool timeAdjustment_Set;

		private bool telescopeAperture_Set;

		private bool sNR_Set;

		private bool weight_D_Set;

		private bool weight_R_Set;

		private int longitude_Sec_N;

		private int latitude_Sec_N;

		private int s_Dis_N;

		private int accuracy_D_N;

		private int pEQ_D_N;

		private int s_Reap_N;

		private int accuracy_R_N;

		private int pEQ_R_N;

		private double yat0;

		private double sNR;

		private double observerMotion_X_D;

		private double observerMotion_Y_D;

		private double observerMotion_X_R;

		private double observerMotion_Y_R;

		private static List<string[]> CorrectCodes = new List<string[]>();

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

		public string Observer1
		{
			get
			{
				return observer1;
			}
			set
			{
				observer1 = value;
			}
		}

		public string Observer2
		{
			get
			{
				return observer2;
			}
			set
			{
				observer2 = value;
			}
		}

		public string ObserversAll
		{
			get
			{
				return observersAll;
			}
			set
			{
				observersAll = value;
			}
		}

		public bool MoreThan2Observers
		{
			get
			{
				return moreThan2Observers;
			}
			set
			{
				moreThan2Observers = value;
			}
		}

		public string NearTo
		{
			get
			{
				return nearTo;
			}
			set
			{
				nearTo = value;
			}
		}

		public string StateCountry
		{
			get
			{
				return stateCountry;
			}
			set
			{
				stateCountry = value;
			}
		}

		public bool Longitude_PlusSign
		{
			get
			{
				return longitude_SignIsPlus;
			}
			set
			{
				longitude_SignIsPlus = value;
			}
		}

		public int Longitude_Deg
		{
			get
			{
				return longitude_Deg;
			}
			set
			{
				longitude_Deg = value;
			}
		}

		public int Longitude_Min
		{
			get
			{
				return longitude_Min;
			}
			set
			{
				longitude_Min = value;
			}
		}

		public double Longitude_Sec
		{
			get
			{
				return longitude_Sec;
			}
			set
			{
				longitude_Sec = value;
			}
		}

		public int Longitude_Sec_N
		{
			get
			{
				return longitude_Sec_N;
			}
			set
			{
				longitude_Sec_N = value;
			}
		}

		public bool Latitude_PlusSign
		{
			get
			{
				return latitude_SignisPlus;
			}
			set
			{
				latitude_SignisPlus = value;
			}
		}

		public int Latitude_Deg
		{
			get
			{
				return latitude_Deg;
			}
			set
			{
				latitude_Deg = value;
			}
		}

		public int Latitude_Min
		{
			get
			{
				return latitude_Min;
			}
			set
			{
				latitude_Min = value;
			}
		}

		public double Latitude_Sec
		{
			get
			{
				return latitude_Sec;
			}
			set
			{
				latitude_Sec = value;
			}
		}

		public int Latitude_Sec_N
		{
			get
			{
				return latitude_Sec_N;
			}
			set
			{
				latitude_Sec_N = value;
			}
		}

		public int Altitude
		{
			get
			{
				return altitude;
			}
			set
			{
				altitude = value;
			}
		}

		public bool Altitude_Set
		{
			get
			{
				return altitude_Set;
			}
			set
			{
				altitude_Set = value;
			}
		}

		public string Datum
		{
			get
			{
				return datum;
			}
			set
			{
				datum = value;
			}
		}

		public int TelescopeAperture
		{
			get
			{
				return (int)telescopeAperture;
			}
			set
			{
				telescopeAperture = value;
			}
		}

		public bool TelescopeAperture_Set
		{
			get
			{
				return telescopeAperture_Set;
			}
			set
			{
				telescopeAperture_Set = value;
			}
		}

		public string TelescopeType
		{
			get
			{
				return telescopeType;
			}
			set
			{
				telescopeType = value;
			}
		}

		public string Stability
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

		public string Transparency
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

		public string TimeSource
		{
			get
			{
				return timeSource;
			}
			set
			{
				timeSource = value;
			}
		}

		public double SNR
		{
			get
			{
				if (sNR > 20.0)
				{
					return 20.0;
				}
				if (sNR < 0.0)
				{
					return 0.0;
				}
				return sNR;
			}
			set
			{
				sNR = value;
			}
		}

		public bool SNR_Set
		{
			get
			{
				return sNR_Set;
			}
			set
			{
				sNR_Set = value;
			}
		}

		public int H_Dis
		{
			get
			{
				return h_Dis;
			}
			set
			{
				h_Dis = value;
			}
		}

		public int M_Dis
		{
			get
			{
				return m_Dis;
			}
			set
			{
				m_Dis = value;
			}
		}

		public double S_Dis
		{
			get
			{
				return s_Dis;
			}
			set
			{
				s_Dis = value;
			}
		}

		public int S_Dis_DecPlaces
		{
			get
			{
				return s_Dis_N;
			}
			set
			{
				s_Dis_N = value;
			}
		}

		public string Event_D
		{
			get
			{
				return event_D;
			}
			set
			{
				event_D = value;
			}
		}

		public double Accuracy_D
		{
			get
			{
				return accuracy_D;
			}
			set
			{
				accuracy_D = value;
			}
		}

		public bool Accuracy_D_Set
		{
			get
			{
				return accuracy_D_Set;
			}
			set
			{
				accuracy_D_Set = value;
			}
		}

		public int Accuracy_D_DecPlaces
		{
			get
			{
				return accuracy_D_N;
			}
			set
			{
				accuracy_D_N = value;
			}
		}

		public double PEQ_D
		{
			get
			{
				return pEQ_D;
			}
			set
			{
				pEQ_D = value;
			}
		}

		public bool PEQ_D_Set
		{
			get
			{
				return pEQ_D_Set;
			}
			set
			{
				pEQ_D_Set = value;
			}
		}

		public int PEQ_D_N
		{
			get
			{
				return pEQ_D_N;
			}
			set
			{
				pEQ_D_N = value;
			}
		}

		public int Weight_D
		{
			get
			{
				return weight_D;
			}
			set
			{
				weight_D = value;
			}
		}

		public bool Weight_D_Set
		{
			get
			{
				return weight_D_Set;
			}
			set
			{
				weight_D_Set = value;
			}
		}

		public int H_Reap
		{
			get
			{
				return h_Reap;
			}
			set
			{
				h_Reap = value;
			}
		}

		public int M_Reap
		{
			get
			{
				return m_Reap;
			}
			set
			{
				m_Reap = value;
			}
		}

		public double S_Reap
		{
			get
			{
				return s_Reap;
			}
			set
			{
				s_Reap = value;
			}
		}

		public int S_Reap_DecPlaces
		{
			get
			{
				return s_Reap_N;
			}
			set
			{
				s_Reap_N = value;
			}
		}

		public string Event_R
		{
			get
			{
				return event_R;
			}
			set
			{
				event_R = value;
			}
		}

		public double Accuracy_R
		{
			get
			{
				return accuracy_R;
			}
			set
			{
				accuracy_R = value;
			}
		}

		public bool Accuracy_R_Set
		{
			get
			{
				return accuracy_R_Set;
			}
			set
			{
				accuracy_R_Set = value;
			}
		}

		public int Accuracy_R_DecPlaces
		{
			get
			{
				return accuracy_R_N;
			}
			set
			{
				accuracy_R_N = value;
			}
		}

		public double PEQ_R
		{
			get
			{
				return pEQ_R;
			}
			set
			{
				pEQ_R = value;
			}
		}

		public bool PEQ_R_Set
		{
			get
			{
				return pEQ_R_Set;
			}
			set
			{
				pEQ_R_Set = value;
			}
		}

		public int PEQ_R_N
		{
			get
			{
				return pEQ_R_N;
			}
			set
			{
				pEQ_R_N = value;
			}
		}

		public int Weight_R
		{
			get
			{
				return weight_R;
			}
			set
			{
				weight_R = value;
			}
		}

		public bool Weight_R_Set
		{
			get
			{
				return weight_R_Set;
			}
			set
			{
				weight_R_Set = value;
			}
		}

		public string Method
		{
			get
			{
				return method;
			}
			set
			{
				method = value;
			}
		}

		public string PlotCode
		{
			get
			{
				return plotCode;
			}
			set
			{
				plotCode = value;
			}
		}

		public double TimeAdjustment
		{
			get
			{
				return timeAdjustment;
			}
			set
			{
				timeAdjustment = value;
			}
		}

		public bool TimeAdjustment_Set
		{
			get
			{
				return timeAdjustment_Set;
			}
			set
			{
				timeAdjustment_Set = value;
			}
		}

		public string FreeText
		{
			get
			{
				return freeText;
			}
			set
			{
				freeText = value;
			}
		}

		public double MinimumDistance
		{
			get
			{
				return yat0;
			}
			set
			{
				yat0 = value;
			}
		}

		public string FormattedLongitude => Formatted_Longitude();

		public string FormattedLatitude => Formatted_Latitude();

		public double Longitude => longitude();

		public double Latitude => latitude();

		public double T_Disappear => (double)h_Dis + (double)m_Dis / 60.0 + s_Dis / 3600.0;

		public double T_Reappear => (double)h_Reap + (double)m_Reap / 60.0 + s_Reap / 3600.0;

		public string Formatted_T_Disappear => Utilities.DEGtoDMS((double)h_Dis + (double)m_Dis / 60.0 + s_Dis / 3600.0, 2, S_Dis_DecPlaces, MinutesOnly: false);

		public string Formatted_T_Reappear => Utilities.DEGtoDMS((double)h_Reap + (double)m_Reap / 60.0 + s_Reap / 3600.0, 2, S_Reap_DecPlaces, MinutesOnly: false);

		public double ObserverMotion_AlongPath_D
		{
			get
			{
				return observerMotion_X_D;
			}
			set
			{
				observerMotion_X_D = value;
			}
		}

		public double ObserverMotion_AcrossPath_D
		{
			get
			{
				return observerMotion_Y_D;
			}
			set
			{
				observerMotion_Y_D = value;
			}
		}

		public double ObserverMotion_AlongPath_R
		{
			get
			{
				return observerMotion_X_R;
			}
			set
			{
				observerMotion_X_R = value;
			}
		}

		public double ObserverMotion_AcrossPath_R
		{
			get
			{
				return observerMotion_Y_R;
			}
			set
			{
				observerMotion_Y_R = value;
			}
		}

		public static ObserverData Prediction { get; internal set; }

		public static ObserverData ObserverLine { get; internal set; }

		private string Formatted_Longitude()
		{
			if (longitude_SignIsPlus)
			{
				return string.Format("+{0,3:F0}{1,3:F0}", longitude_Deg, longitude_Min) + Utilities.FormatNumber(longitude_Sec, 3, longitude_Sec_N).PadRight(5);
			}
			return string.Format("-{0,3:F0}{1,3:F0}", longitude_Deg, longitude_Min) + Utilities.FormatNumber(longitude_Sec, 3, longitude_Sec_N).PadRight(5);
		}

		private string Formatted_Latitude()
		{
			if (latitude_SignisPlus)
			{
				return string.Format("+{0,2:F0}{1,3:F0}", latitude_Deg, latitude_Min) + Utilities.FormatNumber(latitude_Sec, 3, latitude_Sec_N).PadRight(5);
			}
			return string.Format("-{0,2:F0}{1,3:F0}", latitude_Deg, latitude_Min) + Utilities.FormatNumber(latitude_Sec, 3, latitude_Sec_N).PadRight(5);
		}

		private double longitude()
		{
			if (longitude_SignIsPlus)
			{
				return (double)longitude_Deg + (double)longitude_Min / 60.0 + longitude_Sec / 3600.0;
			}
			return 0.0 - ((double)longitude_Deg + (double)longitude_Min / 60.0 + longitude_Sec / 3600.0);
		}

		private double latitude()
		{
			if (latitude_SignisPlus)
			{
				return (double)latitude_Deg + (double)latitude_Min / 60.0 + latitude_Sec / 3600.0;
			}
			return 0.0 - ((double)latitude_Deg + (double)latitude_Min / 60.0 + latitude_Sec / 3600.0);
		}

		internal void ObserversAll_Create()
		{
			ObserversAll = Observer1;
			if ((Observer2.Trim().Length > 0) & MoreThan2Observers)
			{
				ObserversAll = ObserversAll + ", " + Observer2.Trim();
			}
			else if (Observer2.Trim().Length > 0)
			{
				ObserversAll = ObserversAll + " & " + Observer2.Trim();
			}
			if (MoreThan2Observers)
			{
				ObserversAll += " et al";
			}
		}

		internal static string CountryCode(string Country)
		{
			Country = Country.Trim();
			if (Country.Contains(" RSA"))
			{
				return "ZAF";
			}
			if (Country.Contains("ALBE"))
			{
				return "AB";
			}
			if (Country.Contains("ALGIE"))
			{
				return "DZA";
			}
			if (Country.Contains("ARGEN"))
			{
				return "ARG";
			}
			if (Country.Contains("ARG."))
			{
				return "ARG";
			}
			if (Country.Contains("ARUBA"))
			{
				return "ABW";
			}
			if (Country.Contains("AUST."))
			{
				return "AUS";
			}
			if (Country.Contains("AUSTRA"))
			{
				return "AUS";
			}
			if (Country.Contains("AUSTRI"))
			{
				return "AUT";
			}
			if (Country.Contains("BAHA"))
			{
				return "BHS";
			}
			if (Country.Contains("ARGEN"))
			{
				return "ARG";
			}
			if (Country.Contains("BELA"))
			{
				return "BLR";
			}
			if (Country.Contains("BELG"))
			{
				return "BEL";
			}
			if (Country.Contains("BRAZ"))
			{
				return "BRA";
			}
			if (Country.Contains("CANA"))
			{
				return "CAN";
			}
			if (Country.Contains("CHIL"))
			{
				return "CHL";
			}
			if (Country.Contains("CHIN"))
			{
				return "CHN";
			}
			if (Country.Contains("CZECH"))
			{
				return "CZE";
			}
			if (Country.Contains("DENM"))
			{
				return "DNK";
			}
			if (Country.Contains("ENGL"))
			{
				return "GBR";
			}
			if (Country.Contains("EQUAD"))
			{
				return "ECU";
			}
			if (Country.Contains("FINL"))
			{
				return "FIN";
			}
			if (Country.Contains("FLORID"))
			{
				return "FL";
			}
			if (Country.Contains("FRAN"))
			{
				return "FRA";
			}
			if (Country.Contains("GEORG"))
			{
				return "GEO";
			}
			if (Country.Contains("GERM"))
			{
				return "DEU";
			}
			if (Country.Contains("GRANA"))
			{
				return "GRD";
			}
			if (Country.Contains("GREE"))
			{
				return "GRC";
			}
			if (Country.Contains("GUYA"))
			{
				return "GUY";
			}
			if (Country.Contains("HONG K"))
			{
				return "CHN";
			}
			if (Country.Contains("HUNGA"))
			{
				return "HUN";
			}
			if (Country.Contains("INDIA"))
			{
				return "IND";
			}
			if (Country.Contains("IRAN"))
			{
				return "IRN";
			}
			if (Country.Contains("IREL"))
			{
				return "IRL";
			}
			if (Country.Contains("ISRA"))
			{
				return "ISR";
			}
			if (Country.Contains("ITAL"))
			{
				return "ITA";
			}
			if (Country.Contains("JAM"))
			{
				return "JAM";
			}
			if (Country.Contains("LEBA"))
			{
				return "LBN";
			}
			if (Country.Contains("MALT"))
			{
				return "MLT";
			}
			if (Country.Contains("MEXI"))
			{
				return "MEX";
			}
			if (Country.Contains("MOROC"))
			{
				return "MAR";
			}
			if (Country.Contains("NETHE"))
			{
				return "NLD";
			}
			if (Country.Contains("NEW Z"))
			{
				return "NZL";
			}
			if (Country.Contains("NORW"))
			{
				return "NOR";
			}
			if (Country.Contains("BELA"))
			{
				return "BLR";
			}
			if (Country.Contains("NOVA S"))
			{
				return "NS";
			}
			if (Country.Contains("POLA"))
			{
				return "POL";
			}
			if (Country.Contains("PORTU"))
			{
				return "PRT";
			}
			if (Country.Contains("QUEEN"))
			{
				return "QLD";
			}
			if (Country.Contains("RUSSIA"))
			{
				return "RUS";
			}
			if (Country.Contains("SAUD"))
			{
				return "SAU";
			}
			if (Country.Contains("SLOVA"))
			{
				return "SVK";
			}
			if (Country.Contains("SOUTH A"))
			{
				return "ZAF";
			}
			if (Country.Contains("SPAI"))
			{
				return "ESP";
			}
			if (Country.Contains("STH AF"))
			{
				return "ZAF";
			}
			if (Country.Contains("SWED"))
			{
				return "SWE";
			}
			if (Country.Contains("SWITZ"))
			{
				return "CHE";
			}
			if (Country.Contains("TADZ"))
			{
				return "TJK";
			}
			if (Country.Contains("TAIW"))
			{
				return "TWN";
			}
			if (Country.Contains("TOBA"))
			{
				return "TTO";
			}
			if (Country.Contains("TURKE"))
			{
				return "TR";
			}
			if (Country.Contains("U.K."))
			{
				return "GBR";
			}
			if (Country.Contains("UNITED K"))
			{
				return "GBR";
			}
			if (Country.Contains("UKRA"))
			{
				return "UKR";
			}
			if (Country.Contains("URUG"))
			{
				return "URY";
			}
			if (Country.Contains("U.S.S"))
			{
				return "RUS";
			}
			if (Country.Contains("VENEZ"))
			{
				return "VEN";
			}
			if (Country.Contains("VEN."))
			{
				return "VEN";
			}
			if (Country.Contains("VIRG"))
			{
				return "VIR";
			}
			if (Country.Contains("ALAB"))
			{
				return "AL";
			}
			if (Country.Contains("ARIZ"))
			{
				return "AZ";
			}
			if (Country.Contains("ARKAN"))
			{
				return "AR";
			}
			if (Country.Contains("B.C."))
			{
				return "BC";
			}
			if (Country.Contains("CALI"))
			{
				return "CA";
			}
			if (Country.Contains("HAWA"))
			{
				return "HI";
			}
			if (Country.Contains("ILLIN"))
			{
				return "IL";
			}
			if (Country.Contains("IOWA"))
			{
				return "IA";
			}
			if (Country.Contains("KANSA"))
			{
				return "KS";
			}
			if (Country.Contains("LOUISI"))
			{
				return "LA";
			}
			if (Country.Contains("MASS"))
			{
				return "MA";
			}
			if (Country.Contains("MARY"))
			{
				return "MD";
			}
			if (Country.Contains("MAIN"))
			{
				return "ME";
			}
			if (Country.Contains("MISSO"))
			{
				return "MO";
			}
			if (Country.Contains("MISSI"))
			{
				return "MS";
			}
			if (Country.Contains("NEVA"))
			{
				return "NV";
			}
			if (Country.Contains("NEW J"))
			{
				return "NJ";
			}
			if (Country.Contains("NEW M"))
			{
				return "NM";
			}
			if (Country.Contains("NEW Y"))
			{
				return "NY";
			}
			if (Country.Contains("OKLAH"))
			{
				return "OK";
			}
			if (Country.Contains("ONTAR"))
			{
				return "ON";
			}
			if (Country.Contains("OREG"))
			{
				return "OR";
			}
			if (Country.Contains("PENN"))
			{
				return "PA";
			}
			if (Country.Contains("QUEB"))
			{
				return "QC";
			}
			if (Country.Contains("SASK"))
			{
				return "SK";
			}
			if (Country.Contains("SOUTH C"))
			{
				return "SC";
			}
			if (Country.Contains("TEXA"))
			{
				return "TX";
			}
			if (Country.Contains("UTAH"))
			{
				return "UT";
			}
			if (Country.Contains("WASH"))
			{
				return "WA";
			}
			if (Country.Contains("WISC"))
			{
				return "WI";
			}
			if (Country.Contains("LEITER"))
			{
				return "DEU";
			}
			if (CorrectCodes.Count == 0 && File.Exists(Utilities.AppPath + "\\Resource Files\\CorrectOddCountries.csv"))
			{
				using StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\Resource Files\\CorrectOddCountries.csv");
				do
				{
					string[] array = streamReader.ReadLine()!.Split(new char[1] { ',' });
					string[] item = new string[2]
					{
						array[1].Trim(),
						array[2]
					};
					CorrectCodes.Add(item);
				}
				while (!streamReader.EndOfStream);
			}
			for (int i = 0; i < CorrectCodes.Count; i++)
			{
				string[] array2 = CorrectCodes[i];
				if (Country == array2[0])
				{
					return array2[1];
				}
			}
			return Country;
		}

		public void DecodeOldObserverLine(string Line)
		{
			if (!int.TryParse(Line.Substring(0, 3), out seqNumber))
			{
				seqNumber = 0;
			}
			string text = Line.Substring(3, 37);
			if (text.ToLower().Contains("et al"))
			{
				text = text.Replace("et al", "").Replace("Et Al", "").Replace("Et al", "");
				MoreThan2Observers = true;
			}
			longitude_SignIsPlus = !Line.Substring(41, 4).Contains("-");
			if (!int.TryParse(Line.Substring(42, 3).Replace('-', ' ').Replace('+', ' '), out longitude_Deg))
			{
				longitude_Deg = 0;
			}
			if (!int.TryParse(Line.Substring(45, 2), out longitude_Min))
			{
				longitude_Min = 0;
			}
			if (!double.TryParse(Line.Substring(47, 5), out longitude_Sec))
			{
				longitude_Sec = 0.0;
			}
			longitude_Sec_N = Utilities.DecimalPlaces(Line.Substring(47, 5));
			latitude_SignisPlus = !Line.Substring(52, 3).Contains("-");
			if (!int.TryParse(Line.Substring(53, 2).Replace('-', ' ').Replace('+', ' '), out latitude_Deg))
			{
				latitude_Deg = 0;
			}
			if (!int.TryParse(Line.Substring(55, 2), out latitude_Min))
			{
				latitude_Min = 0;
			}
			if (!double.TryParse(Line.Substring(57, 4), out latitude_Sec))
			{
				latitude_Sec = 0.0;
			}
			latitude_Sec_N = Utilities.DecimalPlaces(Line.Substring(57, 4));
			altitude_Set = int.TryParse(Line.Substring(61, 5), out altitude);
			string text4 = (NearTo = (StateCountry = ""));
			string[] array = text.Split(new char[1] { ',' });
			if (array.Length != 0)
			{
				string[] array2 = array[0].Replace("+/-", "").Replace("\"", "").Replace("?", "")
					.Replace("  ", "")
					.Split(new char[1] { '/' });
				Observer1 = Utilities.ProperCase(array2[0].Trim());
				if (array2.Length > 1)
				{
					Observer2 = Utilities.ProperCase(array2[1]);
				}
				if (array2.Length > 2)
				{
					MoreThan2Observers = true;
				}
				ObserversAll_Create();
			}
			if (array.Length > 1)
			{
				if ((longitude_Deg > -10) & (Longitude_Deg < 60) & (Latitude_Deg > 0))
				{
					StateCountry = array[^1].Trim();
					if (array.Length > 2)
					{
						NearTo = array[1].Trim();
					}
					else
					{
						NearTo = "";
					}
				}
				else if ((longitude_Deg > 100) & (Longitude_Deg < 150) & (Latitude_Deg > 20))
				{
					StateCountry = array[^1].Trim();
					if (array.Length > 3)
					{
						NearTo = array[1].Trim() + ":" + array[2].Trim();
					}
					else if (array.Length > 2)
					{
						NearTo = array[1].Trim();
					}
					else
					{
						NearTo = "";
					}
					if (StateCountry.Contains("Ja"))
					{
						StateCountry = "JP";
					}
				}
				else
				{
					if (array.Length > 1)
					{
						NearTo = array[1].Trim();
					}
					if (array.Length > 2)
					{
						StateCountry = array[^1].Trim();
					}
				}
			}
			StateCountry = CountryCode(StateCountry.ToUpper());
			if (Observer1.ToUpper().Contains("PREDIC"))
			{
				Observer1 = "Predicted";
				Observer2 = "";
				MoreThan2Observers = false;
				NearTo = "";
				StateCountry = "";
			}
			Datum = Line.Substring(66, 1);
			TelescopeAperture_Set = double.TryParse(Line.Substring(67, 3), out telescopeAperture);
			TelescopeType = Line.Substring(70, 1);
			Stability = Line.Substring(71, 1);
			Transparency = Line.Substring(72, 1);
			string text5 = Line.Substring(73, 1);
			if ((text5 == "1") | (text5 == "2"))
			{
				TimeSource = "a";
			}
			else if ((text5 == "3") | (text5 == "4") | (text5 == "5") | (text5 == "7"))
			{
				TimeSource = "d";
			}
			else if (text5 == "6")
			{
				TimeSource = "f";
			}
			else if ((text5 == "8") | (text5 == "9"))
			{
				TimeSource = "g";
			}
			else
			{
				TimeSource = text5;
			}
			if (!int.TryParse(Line.Substring(74, 2), out h_Dis))
			{
				h_Dis = 0;
			}
			if (!int.TryParse(Line.Substring(76, 2), out m_Dis))
			{
				M_Dis = 0;
			}
			if (!double.TryParse(Line.Substring(78, 5), out s_Dis))
			{
				s_Dis = 0.0;
			}
			s_Dis_N = Utilities.DecimalPlaces(Line.Substring(78, 5));
			event_D = Line.Substring(83, 1);
			accuracy_D_Set = double.TryParse(Line.Substring(84, 4), out accuracy_D);
			accuracy_D_N = Utilities.DecimalPlaces(Line.Substring(84, 4));
			if (accuracy_D_N > 2)
			{
				accuracy_D_N = 2;
			}
			pEQ_D_Set = double.TryParse(Line.Substring(88, 4), out pEQ_D);
			pEQ_D_N = Utilities.DecimalPlaces(Line.Substring(88, 4));
			if (pEQ_D_N > 2)
			{
				pEQ_D_N = 2;
			}
			weight_D_Set = int.TryParse(Line.Substring(92, 1), out weight_D);
			if (weight_D > 5)
			{
				weight_D = 5;
			}
			if (!int.TryParse(Line.Substring(93, 2), out h_Reap))
			{
				h_Reap = 0;
			}
			if (!int.TryParse(Line.Substring(95, 2), out m_Reap))
			{
				M_Reap = 0;
			}
			if (!double.TryParse(Line.Substring(97, 5), out s_Reap))
			{
				s_Reap = 0.0;
			}
			s_Reap_N = Utilities.DecimalPlaces(Line.Substring(97, 5));
			event_R = Line.Substring(102, 1);
			accuracy_R_Set = double.TryParse(Line.Substring(103, 4), out accuracy_R);
			accuracy_R_N = Utilities.DecimalPlaces(Line.Substring(103, 4));
			if (accuracy_R_N > 2)
			{
				accuracy_R_N = 2;
			}
			pEQ_R_Set = double.TryParse(Line.Substring(107, 4), out pEQ_R);
			pEQ_R_N = Utilities.DecimalPlaces(Line.Substring(107, 4));
			if (pEQ_R_N > 2)
			{
				pEQ_R_N = 2;
			}
			weight_R_Set = int.TryParse(Line.Substring(111, 1), out weight_R);
			if (weight_R > 5)
			{
				weight_R = 5;
			}
			if (event_D == "P")
			{
				double num = ((double)(H_Dis + H_Reap) + (double)(M_Dis + M_Reap) / 60.0 + (S_Dis + S_Reap) / 3600.0) / 2.0;
				int num4 = (H_Dis = (H_Reap = (int)Math.Floor(num)));
				num = (num - (double)H_Dis) * 60.0;
				num4 = (M_Dis = (M_Reap = (int)Math.Floor(num)));
				double num9 = (S_Dis = (S_Reap = (int)((num - (double)M_Dis) * 60.0)));
			}
			string text6 = Line.Substring(112, 1);
			if ((text6 == "0") | (text6 == "1") | (text6 == "2"))
			{
				Method = "f";
			}
			else if ((text6 == "3") | (text6 == "4"))
			{
				Method = "a";
			}
			else
			{
				switch (text6)
				{
				case "5":
					Method = "c";
					break;
				case "6":
					Method = "e";
					break;
				case "7":
					Method = " ";
					break;
				default:
					Method = text6;
					break;
				}
			}
			plotCode = Line.Substring(113, 1);
			timeAdjustment_Set = double.TryParse(Line.Substring(114, 5), out timeAdjustment);
			FreeText = Line.Substring(119, 30).Replace('|', ' ');
			int num10 = freeText.ToLower().IndexOf("snr");
			if (num10 >= 0)
			{
				int num11 = FreeText.IndexOf("=") + 1;
				int num12 = FreeText.IndexOf(" ", num11);
				if (num12 < 0)
				{
					num12 = FreeText.Length;
				}
				if (double.TryParse(FreeText.Substring(num11, num12 - num11), out var result))
				{
					SNR = result;
					SNR_Set = true;
					FreeText = FreeText.Remove(num10, num12 - num10);
				}
				else
				{
					SNR = 0.0;
					SNR_Set = false;
				}
			}
		}

		internal string EncodeObserverInEditorToFixedFormatLine()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.AppendFormat("{0,3:F0}", seqNumber);
			StringBuilder stringBuilder2 = new StringBuilder();
			stringBuilder2.Append(Observer1.Trim().ToString());
			if (Observer2.Trim() != "")
			{
				stringBuilder2.Append("/" + Observer2.Trim().ToString());
			}
			if (moreThan2Observers)
			{
				stringBuilder2.Append(" et al");
			}
			stringBuilder2.Append("," + NearTo + "," + StateCountry);
			stringBuilder.Append(stringBuilder2.ToString().PadRight(38).Substring(0, 38));
			if (longitude_SignIsPlus)
			{
				stringBuilder.Append("+");
			}
			else
			{
				stringBuilder.Append("-");
			}
			stringBuilder.AppendFormat("{0,3:F0}", longitude_Deg);
			stringBuilder.AppendFormat("{0,2:F0}", longitude_Min);
			if (longitude_Sec_N == -1)
			{
				stringBuilder.Append("  .  ");
			}
			else
			{
				stringBuilder.Append(Utilities.FormatNumber(longitude_Sec, 2, longitude_Sec_N).PadRight(5));
			}
			if (latitude_SignisPlus)
			{
				stringBuilder.Append("+");
			}
			else
			{
				stringBuilder.Append("-");
			}
			stringBuilder.AppendFormat("{0,2:F0}", latitude_Deg);
			stringBuilder.AppendFormat("{0,2:F0}", latitude_Min);
			if (latitude_Sec_N == -1)
			{
				stringBuilder.Append("  . ");
			}
			else
			{
				stringBuilder.Append(Utilities.FormatNumber(latitude_Sec, 2, latitude_Sec_N).PadRight(4));
			}
			if (altitude_Set)
			{
				stringBuilder.AppendFormat("{0,5:F0}", altitude);
			}
			else
			{
				stringBuilder.Append("     ");
			}
			stringBuilder.Append(datum);
			if (telescopeAperture_Set)
			{
				if (telescopeAperture > 900.0)
				{
					stringBuilder.AppendFormat(">9m");
				}
				else
				{
					stringBuilder.AppendFormat("{0,3:F0}", telescopeAperture);
				}
			}
			else
			{
				stringBuilder.Append("   ");
			}
			stringBuilder.Append(TelescopeType);
			stringBuilder.Append(Stability);
			stringBuilder.Append(Transparency);
			stringBuilder.Append(TimeSource);
			if (T_Disappear != 0.0)
			{
				stringBuilder.AppendFormat("{0,2:F0}", h_Dis);
				stringBuilder.AppendFormat("{0,2:F0}", m_Dis);
				stringBuilder.Append(Utilities.FormatNumber(s_Dis, 2, s_Dis_N).PadRight(5).Substring(0, 5));
			}
			else
			{
				stringBuilder.Append("      .  ");
			}
			stringBuilder.Append(event_D);
			if (accuracy_D_Set)
			{
				stringBuilder.Append(Utilities.FormatNumber(accuracy_D, 1, accuracy_D_N).PadRight(4).Substring(0, 4));
			}
			else
			{
				stringBuilder.Append("".PadRight(4));
			}
			if (pEQ_D_Set)
			{
				stringBuilder.Append(Utilities.FormatNumber(pEQ_D, 1, pEQ_D_N).PadRight(4));
			}
			else
			{
				stringBuilder.Append("".PadRight(4));
			}
			if (weight_D_Set)
			{
				stringBuilder.AppendFormat("{0,1:F0}", weight_D);
			}
			else
			{
				stringBuilder.Append(" ");
			}
			if (T_Reappear != 0.0)
			{
				stringBuilder.AppendFormat("{0,2:F0}", h_Reap);
				stringBuilder.AppendFormat("{0,2:F0}", m_Reap);
				stringBuilder.Append(Utilities.FormatNumber(s_Reap, 2, s_Reap_N).PadRight(5).Substring(0, 5));
			}
			else
			{
				stringBuilder.Append("      .  ");
			}
			stringBuilder.Append(event_R);
			if (accuracy_R_Set)
			{
				stringBuilder.Append(Utilities.FormatNumber(accuracy_R, 1, accuracy_R_N).PadRight(4).Substring(0, 4));
			}
			else
			{
				stringBuilder.Append("".PadRight(4));
			}
			if (pEQ_R_Set)
			{
				stringBuilder.Append(Utilities.FormatNumber(pEQ_R, 1, pEQ_R_N).PadRight(4));
			}
			else
			{
				stringBuilder.Append("".PadRight(4));
			}
			if (weight_R_Set)
			{
				stringBuilder.AppendFormat("{0,1:F0}", weight_R);
			}
			else
			{
				stringBuilder.Append(" ");
			}
			stringBuilder.Append(method);
			stringBuilder.Append(plotCode);
			if (timeAdjustment_Set)
			{
				stringBuilder.Append(string.Format("{0,5:F2}", timeAdjustment).Substring(0, 5));
			}
			else
			{
				stringBuilder.Append("".PadRight(5));
			}
			stringBuilder.Append(freeText.Substring(0, Math.Min(150, freeText.Length)));
			return stringBuilder.ToString();
		}

		public void DecodePredictionLine(string InLine)
		{
			seqNumber = 0;
			string text3 = (Observer1 = (ObserversAll = "Predicted"));
			Observer2 = "";
			longitude_SignIsPlus = !InLine.Substring(3, 4).Contains("-");
			if (!int.TryParse(InLine.Substring(4, 3).Replace('-', ' ').Replace('+', ' '), out longitude_Deg))
			{
				longitude_Deg = 0;
			}
			if (!int.TryParse(InLine.Substring(8, 2), out longitude_Min))
			{
				longitude_Min = 0;
			}
			if (!double.TryParse(InLine.Substring(11, 5), out longitude_Sec))
			{
				longitude_Sec = 0.0;
			}
			longitude_Sec_N = Utilities.DecimalPlaces(InLine.Substring(11, 5));
			latitude_SignisPlus = !InLine.Substring(16, 3).Contains("-");
			if (!int.TryParse(InLine.Substring(17, 2).Replace('-', ' ').Replace('+', ' '), out latitude_Deg))
			{
				latitude_Deg = 0;
			}
			if (!int.TryParse(InLine.Substring(20, 2), out latitude_Min))
			{
				latitude_Min = 0;
			}
			if (!double.TryParse(InLine.Substring(23, 4), out latitude_Sec))
			{
				latitude_Sec = 0.0;
			}
			latitude_Sec_N = Utilities.DecimalPlaces(InLine.Substring(23, 4));
			altitude_Set = true;
			altitude = 0;
			datum = " ";
			telescopeAperture_Set = false;
			telescopeAperture = 0.0;
			telescopeType = " ";
			stability = " ";
			transparency = " ";
			timeSource = " ";
			if (!int.TryParse(InLine.Substring(28, 2), out h_Dis))
			{
				h_Dis = 0;
			}
			if (!int.TryParse(InLine.Substring(31, 2), out m_Dis))
			{
				M_Dis = 0;
			}
			if (!double.TryParse(InLine.Substring(33, 6), out s_Dis))
			{
				s_Dis = 0.0;
			}
			s_Dis_N = Utilities.DecimalPlaces(InLine.Substring(33, 6));
			bool flag = InLine.Substring(39, 1) == "+";
			if (EventDetails.MidT_forMotions > 12.0 && flag)
			{
				h_Dis += 24;
			}
			h_Reap = h_Dis;
			M_Reap = M_Dis;
			s_Reap = s_Dis;
			s_Reap_N = s_Dis_N;
			event_D = (event_R = "P");
			accuracy_D = (accuracy_R = 0.0);
			accuracy_D_Set = (accuracy_R_Set = false);
			accuracy_D_N = (accuracy_R_N = 0);
			pEQ_D = (pEQ_R = 0.0);
			pEQ_D_Set = (pEQ_R_Set = false);
			pEQ_D_N = (pEQ_R_N = 0);
			weight_D = (weight_R = 0);
			weight_D_Set = (weight_R_Set = false);
			method = " ";
			plotCode = " ";
			timeAdjustment = 0.0;
			timeAdjustment_Set = false;
			freeText = "";
		}

		public string PDS_TimingsLine(bool MarkFieldsWithPipes)
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.AppendFormat("|{0,3:F0}|", seqNumber);
			stringBuilder.Append(Observer1.PadRight(20).Substring(0, 20) + "|");
			if (Observer2.Trim() == "")
			{
				stringBuilder.Append("-".PadRight(20) + "|");
			}
			else
			{
				stringBuilder.Append(Observer2.PadRight(20).Substring(0, 20) + "|");
			}
			if (MoreThan2Observers)
			{
				stringBuilder.Append("1|");
			}
			else
			{
				stringBuilder.Append("0|");
			}
			if (NearTo.Trim().Length > 0)
			{
				stringBuilder.Append(NearTo.PadRight(20).Substring(0, 20) + "|");
			}
			else
			{
				stringBuilder.Append("-".PadRight(20) + "|");
			}
			if (StateCountry.Trim().Length > 0)
			{
				stringBuilder.Append(StateCountry.PadRight(3).Substring(0, 3) + "|");
			}
			else
			{
				stringBuilder.Append("-".PadRight(3) + "|");
			}
			stringBuilder.AppendFormat("{0,11:F6}|", longitude());
			stringBuilder.AppendFormat("{0,10:F6}|", latitude());
			if (altitude_Set)
			{
				stringBuilder.AppendFormat("{0,5:F0}|", altitude);
			}
			else
			{
				stringBuilder.Append("-9999|");
			}
			stringBuilder.Append(datum.Replace(' ', '-') + "|");
			if (telescopeAperture_Set)
			{
				stringBuilder.AppendFormat("{0,6:F0}|", telescopeAperture);
			}
			else
			{
				stringBuilder.Append("-99999|");
			}
			stringBuilder.Append(telescopeType.Replace(' ', '-') + "|");
			stringBuilder.Append(stability.Replace(' ', '-') + "|");
			stringBuilder.Append(transparency.Replace(' ', '-') + "|");
			stringBuilder.Append(timeSource.Replace(' ', '-') + "|");
			if (SNR == 0.0)
			{
				stringBuilder.Append("-9.9|");
			}
			else if (SNR > 20.0)
			{
				stringBuilder.Append("20.0|");
			}
			else
			{
				stringBuilder.AppendFormat("{0,4:F1}|", SNR);
			}
			if (T_Disappear != 0.0)
			{
				stringBuilder.Append(string.Format("{0,2:F0}", h_Dis).Replace(' ', '0') + ":");
				stringBuilder.Append(string.Format("{0,2:F0}", m_Dis).Replace(' ', '0') + ":");
				stringBuilder.Append(string.Format("{0,5:F2}", s_Dis).Replace(' ', '0') + "|");
			}
			else
			{
				stringBuilder.Append("00         |");
			}
			stringBuilder.Append(event_D + "|");
			if (accuracy_D_Set)
			{
				stringBuilder.Append(string.Format("{0,6:F3}|", accuracy_D));
			}
			else
			{
				stringBuilder.Append("-9.999|");
			}
			if (pEQ_D_Set)
			{
				stringBuilder.AppendFormat("{0,5:F2}|", Math.Abs(pEQ_D));
			}
			else
			{
				stringBuilder.Append("-9.99|");
			}
			if (Weight_D_Set)
			{
				stringBuilder.AppendFormat("{0,1:F0}|", Weight_D);
			}
			else
			{
				stringBuilder.Append("-|");
			}
			if (T_Reappear != 0.0)
			{
				stringBuilder.Append(string.Format("{0,2:F0}", h_Reap).Replace(' ', '0') + ":");
				stringBuilder.Append(string.Format("{0,2:F0}", m_Reap).Replace(' ', '0') + ":");
				stringBuilder.Append(string.Format("{0,5:F2}", s_Reap).Replace(' ', '0') + "|");
			}
			else
			{
				stringBuilder.Append("00         |");
			}
			stringBuilder.Append(event_R + "|");
			if (accuracy_R_Set)
			{
				stringBuilder.Append(string.Format("{0,6:F3}|", accuracy_R));
			}
			else
			{
				stringBuilder.Append("-9.999|");
			}
			if (pEQ_R_Set)
			{
				stringBuilder.AppendFormat("{0,5:F2}|", Math.Abs(pEQ_R));
			}
			else
			{
				stringBuilder.Append("-9.99|");
			}
			if (Weight_R_Set)
			{
				stringBuilder.AppendFormat("{0,1:F0}|", Weight_R);
			}
			else
			{
				stringBuilder.Append("-|");
			}
			stringBuilder.Append(method.Replace(' ', '-') + "|");
			stringBuilder.Append(plotCode.Replace(' ', '-') + "|");
			if (Math.Abs(timeAdjustment) > 999.0)
			{
				stringBuilder.Append("-999.99|");
			}
			else if (timeAdjustment_Set)
			{
				stringBuilder.Append(string.Format("{0,7:F2}|", timeAdjustment));
			}
			else
			{
				stringBuilder.Append("-999.99|");
			}
			if (freeText.Trim().Length > 0)
			{
				stringBuilder.Append(freeText.PadRight(50).Substring(0, 50) + "|");
			}
			else
			{
				stringBuilder.Append("-".PadRight(50) + "|");
			}
			if (MarkFieldsWithPipes)
			{
				return stringBuilder.ToString();
			}
			return stringBuilder.ToString().Replace("|", " ");
		}

		public int CompareTo(object other)
		{
			return SortField switch
			{
				0 => SeqNumber.CompareTo(((ObserverData)other).SeqNumber), 
				1 => Observer1.CompareTo(((ObserverData)other).Observer1), 
				2 => Longitude.CompareTo(((ObserverData)other).Longitude), 
				3 => Latitude.CompareTo(((ObserverData)other).Latitude), 
				4 => MinimumDistance.CompareTo(((ObserverData)other).MinimumDistance), 
				5 => Observer1.Substring(2).CompareTo(((ObserverData)other).Observer1.Substring(2)), 
				_ => 0, 
			};
		}

		public void UpdateMissTime(double SecsToAdd)
		{
			if (!((event_D != "M") & (event_D != "n") & (event_D != "C")))
			{
				double num = (double)h_Dis + (double)m_Dis / 60.0 + (s_Dis + SecsToAdd) / 3600.0;
				if (num < 0.0)
				{
					num = 0.0;
				}
				h_Dis = (int)Math.Floor(num);
				double num2 = 60.0 * (num - (double)h_Dis);
				m_Dis = (int)Math.Floor(num2);
				s_Dis = 60.0 * (num2 - (double)m_Dis);
				h_Reap = h_Dis;
				m_Reap = m_Dis;
				s_Reap = s_Dis;
				s_Dis_N = (s_Reap_N = 1);
				event_R = event_D;
			}
		}
	}
}
