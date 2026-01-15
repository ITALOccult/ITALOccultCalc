using System;
using System.Text;

namespace Occult.Lunar_Observations
{
	internal class ObserverLine : IComparable
	{
		private string observerCodeForEvent;

		private string observerName;

		private string observerEmail;

		private string observerStation;

		private string observerCode;

		public static int SortField;

		public static bool UseOldFormat;

		public string ObserverCodeForEvent
		{
			get
			{
				return observerCodeForEvent;
			}
			set
			{
				observerCodeForEvent = value;
			}
		}

		public string ObserverName
		{
			get
			{
				return observerName;
			}
			set
			{
				observerName = value;
			}
		}

		public string ObserverEmail
		{
			get
			{
				return observerEmail;
			}
			set
			{
				observerEmail = value;
			}
		}

		public string ObserverStation
		{
			get
			{
				return observerStation;
			}
			set
			{
				observerStation = value;
			}
		}

		public string ObserverCode
		{
			get
			{
				return observerCode;
			}
			set
			{
				observerCode = value;
			}
		}

		public void DecodeObserverLine(string OLine)
		{
			string text = OLine.PadRight(81);
			observerCodeForEvent = text.Substring(1, 1);
			observerName = text.Substring(4, 25);
			int num = observerName.IndexOf("   ");
			if (num > 0)
			{
				observerName = observerName.Substring(0, num);
			}
			if (observerName.ToUpper() == observerName)
			{
				observerName = Utilities.ProperCase(observerName);
			}
			observerEmail = text.Substring(30).Trim();
			if (!observerEmail.Contains("@"))
			{
				observerEmail = "";
				observerStation = text.Substring(32, 5);
				observerCode = text.Substring(37, 4);
			}
			else
			{
				observerStation = "     ";
				observerCode = "    ";
			}
		}

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append("O");
			stringBuilder.Append(observerCodeForEvent.PadRight(1).Substring(0, 1) + "  ");
			if (UseOldFormat)
			{
				stringBuilder.Append(observerName.PadRight(28).Substring(0, 28));
				stringBuilder.Append(observerStation.PadRight(5).Substring(0, 5));
				stringBuilder.Append(observerCode.PadRight(4).Substring(0, 4));
			}
			else
			{
				stringBuilder.Append(observerName.PadRight(25).Substring(0, 25));
				stringBuilder.Append(" " + observerEmail.PadRight(45));
			}
			return stringBuilder.ToString();
		}

		public int CompareTo(object other)
		{
			return SortField switch
			{
				0 => ObserverCodeForEvent.CompareTo(((ObserverLine)other).ObserverCodeForEvent), 
				1 => ObserverName.CompareTo(((ObserverLine)other).ObserverName), 
				2 => ObserverName.Substring(2).CompareTo(((ObserverLine)other).ObserverName.Substring(2)), 
				_ => 0, 
			};
		}
	}
}
